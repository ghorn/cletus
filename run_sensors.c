/* Copyright 2014 Matt Peddie <peddie@alum.mit.edu>
 *
 * This file is hereby placed in the public domain, or, if your legal
 * system does not recognize such a concept, you may consider it
 * licensed under BSD 3.0.  Use it for good.
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <signal.h>
#include <zmq.h>

#include "./zmq.h"
#include "./comms.h"
#include "./structures.h"
#include "./log.h"
#include "./sensors.h"
#include "./misc.h"

#include "./lisa_messages.h"
#include "./print_output.h"



//#define ALL
//#define DEBUG
#ifdef ALL
#define IMU
#define RC
#define AHRS
#define AIRSPEED
#define GPS
#endif



const double gyro_scale_unit_coef = 0.0139882;
const double acc_scale_unit_coef = 0.0009766;
const double mag_scale_unit_coef = 0.0004883;
const double ahrs_unit_coef = 0.0000305;

char* TAG = "RUN_SENSORS";

/* ZMQ resources */
static void *zctx = NULL;
static void *zsock_sensors = NULL;
static void *zsock_log = NULL;
static void *zsock_lisa_gyro = NULL;
static void *zsock_lisa_mag = NULL;
static void *zsock_lisa_accel = NULL;
static void *zsock_lisa_gps = NULL;
static void *zsock_lisa_rc = NULL;
static void *zsock_lisa_ahrs = NULL;
static void *zsock_lisa_airspeed = NULL;
void *zsock_print = NULL;





/* Error tracking. */
int txfails = 0, rxfails = 0;

static void __attribute__((noreturn)) die(int code) {
    zdestroy(zsock_log, zctx);
    zdestroy(zsock_sensors, NULL);
    //Receive sockets
    zdestroy(zsock_lisa_gyro, NULL);
    zdestroy(zsock_lisa_mag, NULL);
    zdestroy(zsock_lisa_accel, NULL);
    zdestroy(zsock_lisa_gps, NULL);
    zdestroy(zsock_lisa_ahrs, NULL);
    zdestroy(zsock_lisa_airspeed, NULL);
    zdestroy(zsock_print, NULL);




    printf("%d TX fails; %d RX fails.\n", txfails, rxfails);
    printf("Moriturus te saluto!\n");
    exit(code);
}

/* Our signal to GTFO */
static int bail = 0;

static void sigdie(int signum) {
    bail = signum;
}

int main(int argc __attribute__((unused)),
         char **argv __attribute__((unused))) {

    struct sched_param param;
    set_priority(&param, RT_PRIORITY);
    stack_prefault();
    struct timespec t;
    int rt_interval = 50000000; /* 5ms*/


    setbuf(stdin, NULL);
    /* Confignals. */
    if (signal(SIGINT, &sigdie) == SIG_IGN)
        signal(SIGINT, SIG_IGN);
    if (signal(SIGTERM, &sigdie) == SIG_IGN)
        signal(SIGTERM, SIG_IGN);
    if (signal(SIGHUP, &sigdie) == SIG_IGN)
        signal(SIGHUP, SIG_IGN);
    if (signal(SIGABRT, &sigdie) == SIG_IGN)
        signal(SIGABRT, SIG_IGN);

    /* ZMQ setup first. */

    /* Set a low high-water mark (a short queue length, measured in
   * messages) so that a sending PUSH will block if the receiving PULL
   * isn't reading.  In the case of PUB/SUB, we still want a short
   * queue; it will prevent outdated messages from building up.  We
   * also set a small-ish buffer size so that the PUSH/PULL socket
   * pair will block or a PUB/SUB socket pair won't accumulate too
   * many outdated messages. */
#ifdef PUSHPULL
    zsock_sensors = setup_zmq_sender(SENSORS_CHAN, &zctx, ZMQ_PUSH, 1, 500);
#else
    zsock_sensors = setup_zmq_sender(SENSORS_CHAN, &zctx, ZMQ_PUB, 1, 500);
#endif
    if (NULL == zsock_sensors)
        return 1;;
    zsock_log = setup_zmq_sender(LOG_CHAN, &zctx, ZMQ_PUB, 500, 500);
    if (NULL == zsock_log)
        return 1;;
    //TODO: Setting multiple Filters for one socket. Testing requiered
    char filter[2];
    filter[1] = '\0';
    filter[0] = IMU_GYRO_SCALED;
    zsock_lisa_gyro = setup_zmq_receiver(LISA_CHAN,&zctx,ZMQ_SUB,&filter[0],1,INPUT_BUFFER_SIZE);
    if (NULL == zsock_lisa_gyro)
        die(1);
    filter[0] = IMU_ACCEL_SCALED;
    zsock_lisa_accel = setup_zmq_receiver(LISA_CHAN,&zctx,ZMQ_SUB,&filter[0],1,INPUT_BUFFER_SIZE);
    if (NULL == zsock_lisa_accel)
        die(1);
    filter[0] = IMU_MAG_SCALED;
    zsock_lisa_mag = setup_zmq_receiver(LISA_CHAN,&zctx,ZMQ_SUB,&filter[0],1,INPUT_BUFFER_SIZE);
    if (NULL == zsock_lisa_mag)
        die(1);
    zsock_lisa_ahrs = setup_zmq_receiver(LISA_CHAN,&zctx,ZMQ_SUB,NULL,1,INPUT_BUFFER_SIZE);
    if (NULL == zsock_lisa_ahrs)
        die(1);
    filter[0] = AIRSPEED_ETS;
    zsock_lisa_airspeed = setup_zmq_receiver(LISA_CHAN,&zctx,ZMQ_SUB,&filter[0],1,INPUT_BUFFER_SIZE);
    if (NULL == zsock_lisa_airspeed)
        die(1);
    zsock_print = setup_zmq_sender(PRINT_CHAN, &zctx, ZMQ_PUSH, 100, 500);
    if (NULL == zsock_print)
        die(1);

    /* Use big buffers here.  We're just publishing the data for
   * logging, so we don't mind saving some data until the logger can
   * receive it. */
    zsock_log = setup_zmq_sender(LOG_CHAN, &zctx, ZMQ_PUB, 1000, 100000);
    if (NULL == zsock_log)
        die(1);


    /* Sensor data storage. */
    //sensors_t outgoing;


    //*****************************************
    // Define Pollitems for Sending and receiving data
    // Events are set to zero at first so we use only what we need
    // If events is set to ZMQ_POLLIN we listen on the channel
    //*****************************************

    zmq_pollitem_t polls[] = {
        //Sending polls
        {
            .socket=zsock_sensors,
            .fd=-1,
            .events=0,
            .revents=0
        },
        {
            .socket = zsock_log,
            .fd = -1,
            .events = 0,
            .revents = 0
        },
        //Receive Sockets
        {
            .socket=zsock_lisa_gyro,
            .fd=-1,
            .events=0,
            .revents=0
        },
        {
            .socket=zsock_lisa_mag,
            .fd=-1,
            .events=0,
            .revents=0
        },
        {
            .socket=zsock_lisa_accel,
            .fd=-1,
            .events=0,
            .revents=0
        },
        {
            .socket=zsock_lisa_gps,
            .fd=-1,
            .events=0,
            .revents=0
        },
        {
            .socket=zsock_lisa_ahrs,
            .fd=-1,
            .events=0,
            .revents=0
        },
        {
            .socket=zsock_lisa_airspeed,
            .fd=-1,
            .events=0,
            .revents=0
        },
        {
            .socket=zsock_lisa_rc,
            .fd=-1,
            .events=0,
            .revents=0
        }
    };

    zmq_pollitem_t *poll_sensors = &polls[0];
#ifdef LOG
    zmq_pollitem_t *poll_log = &polls[1];
#endif
#ifdef IMU
    zmq_pollitem_t *poll_lisa_gyro = &polls[2];
    poll_lisa_gyro->events = ZMQ_POLLIN;
    zmq_pollitem_t *poll_lisa_mag = &polls[3];
    poll_lisa_mag->events = ZMQ_POLLIN;
    zmq_pollitem_t *poll_lisa_accel = &polls[4];
    poll_lisa_accel->events = ZMQ_POLLIN;
#endif
#ifdef GPS
    zmq_pollitem_t *poll_lisa_gps = &polls[5];
    poll_lisa_gps->events = ZMQ_POLLIN;
#endif
#ifdef AHRS
    zmq_pollitem_t *poll_lisa_ahrs = &polls[6];
    poll_lisa_ahrs->events = ZMQ_POLLIN;
#endif
#ifdef RC
    zmq_pollitem_t *poll_lisa_rc = &polls[8];
    poll_lisa_rc->events = ZMQ_POLLIN;
#endif
#ifdef AIRSPEED
    zmq_pollitem_t *poll_lisa_airspeed = &polls[7];
    poll_lisa_airspeed->events = ZMQ_POLLIN;
#endif
    //zmq_pollitem_t *poll_log = &polls[5];


    /*******************************************
 *PROTOBUF-C INITIALIZATION
 *Memory for submessages is allocated here.
 *If submessage is conatined in finally message
 *the pointer will set to the corresponding submessages
 */
    //Initializing Protobuf messages main sensor message
    Protobetty__Sensors sensors = PROTOBETTY__SENSORS__INIT;
#ifdef IMU
    //Initialize Protobuf for Gyro
    Protobetty__Gyro gyro =PROTOBETTY__GYRO__INIT;
    Protobetty__Timestamp gyro_timestamp = PROTOBETTY__TIMESTAMP__INIT;
    Protobetty__Xyz gyro_data = PROTOBETTY__XYZ__INIT;
    gyro.data = &gyro_data;
    gyro.timestamp = &gyro_timestamp;
    //Initialize Protobuf for Accelerometer
    Protobetty__Accel accel = PROTOBETTY__ACCEL__INIT;
    Protobetty__Timestamp accel_timestamp = PROTOBETTY__TIMESTAMP__INIT;
    Protobetty__Xyz accel_data = PROTOBETTY__XYZ__INIT;
    accel.data = &accel_data;
    accel.timestamp = &accel_timestamp;
    //Initialize Protobuf for Magnetometer
    Protobetty__Mag mag = PROTOBETTY__MAG__INIT;
    Protobetty__Timestamp mag_timestamp = PROTOBETTY__TIMESTAMP__INIT;
    Protobetty__Xyz mag_data = PROTOBETTY__XYZ__INIT;
    mag.data = &mag_data;
    mag.timestamp = &mag_timestamp;
#endif
#ifdef AIRSPEED
    //Initialize Protobuf for Airspeed
    Protobetty__Airspeed airspeed = PROTOBETTY__AIRSPEED__INIT;
    Protobetty__Timestamp airspeed_timestamp = PROTOBETTY__TIMESTAMP__INIT;
    airspeed.timestamp = &airspeed_timestamp;
#endif
#ifdef GPS
    //Initialize Protobuf for GPS
    Protobetty__Gps gps = PROTOBETTY__GPS__INIT;
    Protobetty__Timestamp gps_timestamp = PROTOBETTY__TIMESTAMP__INIT;
    gps.timestamp = &gps_timestamp;
    Protobetty__Xyz gps_pos = PROTOBETTY__XYZ__INIT;
    gps.pos = &gps_pos;
    Protobetty__Xyz gps_vel = PROTOBETTY__XYZ__INIT;
    gps.vel = &gps_vel;
#endif




    uint8_t zmq_buffer[PROTOBETTY__MESSAGE__CONSTANTS__MAX_MESSAGE_SIZE];
    void* zmq_buffer_ptr = &zmq_buffer;
    unsigned int packed_length;


    lisa_messages_t data_container;
    lisa_messages_t* const data_ptr = &data_container;



    const int npolls = sizeof(polls) / sizeof(polls[0]);

    clock_gettime(CLOCK_MONOTONIC ,&t);
    /* start after one second */
    t.tv_sec++;

    /* const int noutputs = npolls - ninputs; */

    /* Here's the main loop -- we only do stuff when input or output
   * happens.  The timeout can be put to good use, or you can also use
   * timerfd_create() to create a file descriptor with a timer under
   * the hood and dispatch on that.
   *
   * I apologize for the length of this loop.  For production code,
   * you'd want to pull most of the actual handling out into functions
   * and simply loop over your polls; I've left it all inline here
   * mostly out of laziness. */
    for (;;) {

        /* wait until next shot */
        clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME, &t, NULL);
        //Poll on activated channels for messages
        if (bail) die(bail);
        /* Poll for activity; time out after 10 milliseconds. */
        const int polled = zmq_poll(polls, npolls, 0);
        if (polled < 0) {
            if (bail) die(bail);
            printf("while polling");
            calc_next_shot(&t,rt_interval);
            continue;
        } else if (polled == 0) {
            if (bail) die(bail);
            calc_next_shot(&t,rt_interval);
            continue;
        }


#ifdef IMU
        //********************************************
        // GYRO data received
        //********************************************
        if (poll_lisa_gyro->revents & ZMQ_POLLIN) {
            const int zr = zmq_recv(zsock_lisa_gyro,(uint8_t*) &data_ptr->imu_raw.imu_gyro,sizeof(gyro_raw_t),0);
            if (zr < (int) sizeof(gyro_raw_t)) {
                printf("couldn't read gyro sensor!");
                rxfails++;
                poll_lisa_gyro->revents =0;
            }
            else {
                copy_timestamp(&(data_ptr->imu_raw.imu_gyro.timestamp),gyro.timestamp);
#ifdef DEBUG
                send_debug(zsock_print,TAG,"Received GYRO (ID:%u) and timestamp %f sec (Latency:%fms) ",
                           data_ptr->imu_raw.imu_gyro.id,
                           floating_ProtoTime(gyro.timestamp),
                           calcCurrentLatencyProto(gyro.timestamp)*1e3);
#endif
#ifdef RAW
                raw_to_protobuf(&(data_ptr->imu_raw.imu_gyro.data),&(protobuf_ptr->gyro->data);

        #else
                scaled_to_protobuf(&(data_ptr->imu_raw.imu_gyro.data), gyro.data, gyro_scale_unit_coef);

#endif
            }
        }

        //********************************************
        // ACCELERATION data received
        //********************************************
        if (poll_lisa_accel->revents & ZMQ_POLLIN) {
            const int zr = zmq_recv(zsock_lisa_accel,(uint8_t*) &data_ptr->imu_raw.imu_accel,sizeof(accel_raw_t),0);
            if (zr < (int) sizeof(accel_raw_t)) {
                err("couldn't read sensors!");
                rxfails++;
                poll_lisa_accel->revents =0;
            }
            else {
                copy_timestamp(&(data_ptr->imu_raw.imu_accel.timestamp),accel.timestamp);
#ifdef DEBUG
                send_debug(zsock_print,TAG,"Received ACCELERATION (ID:%i) and timestamp %f sec (Latency:%fms) ",
                           data_ptr->imu_raw.imu_accel.id,
                           floating_ProtoTime(accel.timestamp),
                           calcCurrentLatencyProto(accel.timestamp)*1e3);
#endif
#ifdef RAW
                raw_to_protobuf(&(data_ptr->imu_raw.imu_accel.data),&(protobuf_ptr->accel->data);

        #else
                scaled_to_protobuf(&(data_ptr->imu_raw.imu_accel.data), accel.data, acc_scale_unit_coef);

#endif
            }
        }


        //********************************************
        // MAGNETOMETER data received
        //********************************************
        if (poll_lisa_mag->revents & ZMQ_POLLIN) {
            const int zr = zmq_recv(zsock_lisa_mag,(uint8_t*) &data_ptr->imu_raw.imu_mag,sizeof(mag_raw_t),0);
            if (zr < (int) sizeof(mag_raw_t)) {
                err("couldn't read sensors!");
                rxfails++;
                poll_lisa_mag->revents =0;
            }
            else {
                copy_timestamp(&(data_ptr->imu_raw.imu_mag.timestamp),mag.timestamp);
#ifdef DEBUG
                send_debug(zsock_print,TAG,"Received MAGNETOMETER (ID:%i) and timestamp %f sec (Latency:%fms) ",
                           data_ptr->imu_raw.imu_mag.id,
                           floating_ProtoTime(mag.timestamp),
                           calcCurrentLatencyProto(mag.timestamp)*1e3);
#endif

#ifdef RAW
                raw_to_protobuf(&(data_ptr->imu_raw.imu_mag.data),&(protobuf_ptr->mag->data);

        #else
                scaled_to_protobuf(&(data_ptr->imu_raw.imu_mag.data), mag.data, mag_scale_unit_coef);
#endif
            }
        }





        //********************************************
        // SENDING IMU DATA to Controller
        //********************************************
        if (poll_lisa_gyro->revents > 0 && poll_lisa_mag->revents > 0 && poll_lisa_accel->revents > 0)
        {
            sensors.accel = &accel;
            sensors.gyro = &gyro;
            sensors.mag = &mag;
#if defined(AIRSPEED) && defined(GPS)
            if (poll_lisa_airspeed->revents > 0 && poll_lisa_gps->revents > 0)
            {
                sensors.type = PROTOBETTY__SENSORS__TYPE__IMU_GPS_AIRSPEED;
                sensors.airspeed = &airspeed;
                sensors.gps = &gps;
            }
            else if (poll_lisa_airspeed->revents > 0)
            {
                sensors.type = PROTOBETTY__SENSORS__TYPE__IMU_AIRSPEED;
                sensors.airspeed = &airspeed;
            }
            else if (poll_lisa_gps->revents > 0)
            {
                sensors.type = PROTOBETTY__SENSORS__TYPE__IMU_GPS;
                sensors.gps = &gps;
            }
            else
                sensors.type = PROTOBETTY__SENSORS__TYPE__IMU_ONLY;
#else
#ifdef AIRSPEED
            if (poll_lisa_airspeed->revents > 0)
            {
                sensors.type = PROTOBETTY__SENSORS__TYPE__IMU_AIRSPEED;
                sensors.airspeed = &airspeed;
            }
            else
                sensors.type = SENSORS_PROTO__TYPE__IMU_ONLY;
#endif
#ifdef GPS
            if (poll_lisa_gps->revents > 0)
            {
                sensors.type = PROTOBETTY__SENSORS__TYPE__IMU_GPS;
                sensors.gps = &gps;
            }
            else
                sensors.type = PROTOBETTY__SENSORS__TYPE__IMU_ONLY;
#endif

            sensors.type = PROTOBETTY__SENSORS__TYPE__IMU_ONLY;
#endif

            //get size of packed data
            packed_length = protobetty__sensors__get_packed_size(&sensors);
            //pack data to buffer
            protobetty__sensors__pack(&sensors,zmq_buffer);


            const int zs = zmq_send(zsock_sensors, zmq_buffer_ptr, packed_length, 0);

            if (zs < 0) {
                txfails++;
            } else {
                send_debug(zsock_print,TAG,"IMU sent to controller!, size: %u\n", packed_length);
                poll_sensors->events = 0;
            }
            //Resetting
            poll_sensors->revents = 0;
            poll_lisa_gyro->revents = 0;
            poll_lisa_mag->revents = 0;
            poll_lisa_accel->revents = 0;
            poll_lisa_airspeed->revents = 0;
            poll_lisa_gps->revents = 0;
            protobetty__sensors__init(&sensors);
            calc_next_shot(&t,rt_interval);


        }



#endif

    }

    /* Shouldn't get here. */
    return 0;
}
