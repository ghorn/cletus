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
#include "./uart.h"

#include "./lisa_messages.h"
#include "./print_output.h"

#include "./protos_c/messages.pb-c.h"



//#define ALL
//#define DEBUG
#ifdef ALL
#define IMU
#define RC
#define AHRS
#define AIRSPEED
#define GPS
#endif

/*
 * Callback functions to interpret SBP messages.
 * Every message ID has a callback associated with it to
 * receive and interpret the message payload.
 */
void piksi_pos_llh_callback(u_int16_t sender_id __attribute__((unused)), u_int8_t len __attribute__((unused)), u8 msg[], void *context __attribute__((unused)));
void piksi_heartbeat_callback(u_int16_t sender_id __attribute__((unused)), u_int8_t len __attribute__((unused)), u8 msg[], void *context __attribute__((unused)));
void piksi_baseline_ned_callback(u_int16_t sender_id __attribute__((unused)), u_int8_t len __attribute__((unused)), u8 msg[], void *context __attribute__((unused)));
void piksi_vel_ned_callback(u_int16_t sender_id __attribute__((unused)), u_int8_t len __attribute__((unused)), u8 msg[], void *context __attribute__((unused)));
void piksi_dops_callback(u_int16_t sender_id __attribute__((unused)), u_int8_t len __attribute__((unused)), u8 msg[], void *context __attribute__((unused)));
void piksi_gps_time_callback(u_int16_t sender_id __attribute__((unused)), u_int8_t len __attribute__((unused)), u8 msg[], void *context __attribute__((unused)));
void complete_gps_message(void);


const double gyro_scale_unit_coef = 0.0139882;
const double acc_scale_unit_coef = 0.0009766;
const double mag_scale_unit_coef = 0.0004883;
const double ahrs_unit_coef = 0.0000305;

char* TAG = "RUN_SENSORS";

/* ZMQ resources */
static void *zctx = NULL;
static void *zsock_sensors = NULL;
static void *zsock_log = NULL;

void *zsock_print = NULL;


Protobetty__Sensors sensors;
Protobetty__Gps gps;
Protobetty__LogMessage log_data;


/* Error tracking. */
int txfails = 0, rxfails = 0;

static void __attribute__((noreturn)) die(int code) {
    zdestroy(zsock_log, NULL);
    zdestroy(zsock_sensors, NULL);
    zdestroy(zsock_print, NULL);


    serial_port_close();
    close_serial_port();


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

    struct timespec t;
    struct sched_param param;
    int rt_interval= 0;
    //get arguments for frequency and
    if (argc == 3)
    {
        char* arg_ptr;
        long priority = strtol(argv[1], &arg_ptr,10);
        if (*arg_ptr != '\0' || priority > INT_MAX) {
            printf("Failed to read passed priority. Using DEFAULT value instead.\n");
            priority = DEFAULT_RT_PRIORITY;

        }
        printf("Setting priority to %li\n", priority);
        set_priority(&param, priority);

        long frequency = strtol(argv[2], &arg_ptr,10);
        if (*arg_ptr != '\0' || frequency > INT_MAX) {
            printf("Failed to read passed frequency. Using DEFAULT value instead.\n");
            frequency = DEFAULT_RT_FREQUENCY;
        }
        printf("Setting frequency to %li Hz. RT-interval: %i\n", frequency, rt_interval);
        rt_interval = (NSEC_PER_SEC/frequency);
    }
    else
    {
        printf("No paarameters passed. Using DEFAULT values: \nPRIORITY=%i and FREQUENCY=%i\n",
               DEFAULT_RT_PRIORITY, DEFAULT_RT_FREQUENCY);
        set_priority(&param, DEFAULT_RT_PRIORITY);
        rt_interval = (NSEC_PER_SEC/DEFAULT_RT_FREQUENCY);
    }
    stack_prefault();

    //Init serial port
    int err = serial_port_setup();
    if (err != UART_ERR_NONE)
        printf("Error setting up UART \n");
    //Init Piksi
    const char * const portname = "/dev/ttyUSB0";
    open_serial_port(portname, B1000000, 0, 1 ); // set speed to 1,000,000 bps, 8n1 (no parity) set blocking
    init_message_processing(8192);
    register_velocity_ned_callback(&piksi_baseline_ned_callback);
    register_baseline_ned_callback(&piksi_vel_ned_callback);
    register_position_llh_callback(&piksi_pos_llh_callback);



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
        die(1);
    zsock_print = setup_zmq_sender(PRINT_CHAN, &zctx, ZMQ_PUSH, 100, 500);
    if (NULL == zsock_print)
        die(1);
    zsock_log = setup_zmq_sender(LOG_CHAN, &zctx, ZMQ_PUSH, 100, 500);
    if (NULL == zsock_log)
        die(1);




    /*******************************************
 *PROTOBUF-C INITIALIZATION
 *Memory for submessages is allocated here.
 *If submessage is conatined in finally message
 *the pointer will set to the corresponding submessages
 */
    //Initializing Protobuf messages main sensor message
    protobetty__sensors__init(&sensors);
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
#ifdef RC
    //Initialize Protobuf for RC commands
    Protobetty__Rc rc = PROTOBETTY__RC__INIT;
    Protobetty__Timestamp rc_timestamp = PROTOBETTY__TIMESTAMP__INIT;
    rc.timestamp = &rc_timestamp;
#endif
#ifdef GPS
    //Initialize Protobuf for GPS
    protobetty__gps__init(&gps);
    Protobetty__Timestamp gps_timestamp = PROTOBETTY__TIMESTAMP__INIT;
    gps.timestamp = &gps_timestamp;
#endif
    protobetty__log_message__init(&log_data);
    Protobetty__Servos servos = PROTOBETTY__SERVOS__INIT;
    Protobetty__Timestamp servo_timestamp = PROTOBETTY__TIMESTAMP__INIT;
    servos.timestamp = &servo_timestamp;




    uint8_t* zmq_buffer = calloc(sizeof(uint8_t),PROTOBETTY__MESSAGE__CONSTANTS__MAX_MESSAGE_SIZE);
    unsigned int packed_length;


    lisa_messages_t data_container;
    lisa_messages_t* const data_ptr = &data_container;
    uint8_t msg_length;


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
    if (bail) die(bail);

    int epolldescriptor =  epoll_create1(0);
    if (epolldescriptor == -1)
    {
        perror ("epoll_create");
        abort ();
    }
    epoll_event_t event;
    event.data.fd = serial_stream->fd;
    event.events = EPOLLIN | EPOLLET;
    if (epoll_ctl (epolldescriptor, EPOLL_CTL_ADD, serial_stream->fd, &event) == -1)
    {
        perror ("epoll_ctl");
        abort ();
    }
    uint8_t buffer[256];





    //When sensor data is in circular buffer
    while (1)
    {
        msg_length = read_lisa_message(epolldescriptor,&event, buffer);
        if (bail) die(bail);
        //messages are never longer than 64 bytes
        if ((msg_length < LISA_MAX_MSG_LENGTH) && (msg_length > 0))
        {
            //Check 1: Sender ID must be correct.
            if (buffer[LISA_INDEX_SENDER_ID] == SENDER_ID)
            {
                //Check 2: Checksum must be correct
                if (check_checksum(&buffer[LISA_INDEX_MSG_LENGTH]) == UART_ERR_NONE)
                {
                    send_debug(zsock_print,TAG,"Passed Checksum test. Sending Message [%u bytes] with ID %i\n",
                               msg_length, buffer[LISA_INDEX_MSG_ID]);
                    //Depending on message type copy data and set it to protobuf message
                    switch (buffer[LISA_INDEX_MSG_ID])
                    {
                    case IMU_ALL_SCALED:
                        memcpy(&data_ptr->imu_all,&buffer[LISA_INDEX_MSG_LENGTH], sizeof(imu_all_raw_t));
                        scaled_to_protobuf(&(data_ptr->imu_all.accel), accel.data, acc_scale_unit_coef);
                        scaled_to_protobuf(&(data_ptr->imu_all.gyro), gyro.data, gyro_scale_unit_coef);
                        scaled_to_protobuf(&(data_ptr->imu_all.mag), mag.data, mag_scale_unit_coef);
                        get_protbetty_timestamp(accel.timestamp);
                        gyro.timestamp = mag.timestamp= accel.timestamp;
                        sensors.accel = &accel;
                        sensors.gyro = &gyro;
                        sensors.mag = &mag;
                        send_debug(zsock_print,TAG,"Received IMU_ALL (ID:%u; SeqNo: %u) and timestamp %f sec (Latency:%fms)\nACCEL: X: %f\t Y: %f\t Z: %f \nGYRO: X: %f\t Y: %f\t Z: %f \nMAG: X: %f\t Y: %f\t Z: %f ",
                                   data_ptr->imu_all.header.msg_id,
                                   data_ptr->imu_all.sequence_number,
                                   floating_ProtoTime(accel.timestamp),
                                   calcCurrentLatencyProto(accel.timestamp)*1e3,
                                   accel.data->x, accel.data->y, accel.data->z,
                                   gyro.data->x, gyro.data->y, gyro.data->z,
                                   mag.data->x, mag.data->y, mag.data->z);
                        break;
                    case IMU_ACCEL_SCALED:
                        memcpy(&data_ptr->imu_raw,&buffer[LISA_INDEX_MSG_LENGTH], sizeof(imu_raw_t));
                        scaled_to_protobuf(&(data_ptr->imu_raw.data), accel.data, acc_scale_unit_coef);
                        get_protbetty_timestamp(accel.timestamp);
                        sensors.accel = &accel;
                        send_debug(zsock_print,TAG,"Received ACCEL (ID:%u) and timestamp %f sec (Latency:%fms)\n X: %f\t Y: %f\t Z: %f ",
                                   data_ptr->imu_raw.header.msg_id,
                                   floating_ProtoTime(accel.timestamp),
                                   calcCurrentLatencyProto(accel.timestamp)*1e3,
                                   accel.data->x, accel.data->y, accel.data->z);
                        break;
                    case IMU_GYRO_SCALED:
                        memcpy(&data_ptr->imu_raw,&buffer[LISA_INDEX_MSG_LENGTH], sizeof(imu_raw_t));
                        scaled_to_protobuf(&(data_ptr->imu_raw.data), gyro.data, gyro_scale_unit_coef);
                        get_protbetty_timestamp(gyro.timestamp);
                        sensors.gyro = &gyro;
                        send_debug(zsock_print,TAG,"Received GYRO (ID:%u) and timestamp %f sec (Latency:%fms)\n X: %f\t Y: %f\t Z: %f ",
                                   data_ptr->imu_raw.header.msg_id,
                                   floating_ProtoTime(gyro.timestamp),
                                   calcCurrentLatencyProto(gyro.timestamp)*1e3,
                                   gyro.data->x, gyro.data->y, gyro.data->z);
                        break;

                    case IMU_MAG_SCALED:
                        memcpy(&data_ptr->imu_raw,&buffer[LISA_INDEX_MSG_LENGTH], sizeof(imu_raw_t));
                        scaled_to_protobuf(&(data_ptr->imu_raw.data), mag.data, mag_scale_unit_coef);
                        get_protbetty_timestamp(mag.timestamp);
                        sensors.mag = &mag;
                        send_debug(zsock_print,TAG,"Received MAG (ID:%u) and timestamp %f sec (Latency:%fms)\n X: %f\t Y: %f\t Z: %f",
                                   data_ptr->imu_raw.header.msg_id,
                                   floating_ProtoTime(gyro.timestamp),
                                   calcCurrentLatencyProto(gyro.timestamp)*1e3,
                                   mag.data->x, mag.data->y, mag.data->z);
                        break;
                    case AIRSPEED_ETS:
                        memcpy(&data_ptr->airspeed_raw,&buffer[LISA_INDEX_MSG_LENGTH], sizeof(airspeed_t));
                        airspeed.scaled = data_ptr->airspeed_raw.scaled;
                        get_protbetty_timestamp(airspeed.timestamp);
                        sensors.airspeed = &airspeed;
                        send_debug(zsock_print,TAG,"Received AIRSPEED (ID:%u) and timestamp %f sec (Latency:%fms) ",
                                   data_ptr->airspeed_raw.header.msg_id,
                                   floating_ProtoTime(airspeed.timestamp),
                                   calcCurrentLatencyProto(airspeed.timestamp)*1e3);
                        break;
                    case ROTORCRAFT_RADIO_CONTROL:
                        memcpy(&data_ptr->rc,&buffer[LISA_INDEX_MSG_LENGTH], sizeof(rc_t));
                        rc.rcyaw = data_ptr->rc.yaw;
                        rc.rcthrottle = data_ptr->rc.throttle;
                        rc.rcpitch = data_ptr->rc.pitch;
                        rc.rcroll = data_ptr->rc.roll;
                        rc.rckill = data_ptr->rc.kill;
                        get_protbetty_timestamp(rc.timestamp);
                        log_data.rc = &rc;
                        send_debug(zsock_print,TAG,"Received RC (ID:%u) and timestamp %f sec (Latency:%fms) ",
                                   data_ptr->rc.header.msg_id,
                                   floating_ProtoTime(rc.timestamp),
                                   calcCurrentLatencyProto(rc.timestamp)*1e3);
                        break;
                    case SERVO_COMMANDS:
                        memcpy(&data_ptr->servos_raw,&buffer[LISA_INDEX_MSG_LENGTH], sizeof(servos_t));
                        servos.servo1 = data_ptr->servos_raw.servo_1;
                        servos.servo2 = data_ptr->servos_raw.servo_2;
                        servos.servo3 = data_ptr->servos_raw.servo_3;
                        servos.servo4 = data_ptr->servos_raw.servo_4;
                        servos.servo5 = data_ptr->servos_raw.servo_5;
                        servos.servo6 = data_ptr->servos_raw.servo_6;
                        servos.servo7 = data_ptr->servos_raw.servo_7;
                        get_protbetty_timestamp(servos.timestamp);
                        log_data.servos = &servos;
                        break;
                    default:
                        break;
                    }
                }
                else
                {
                    send_warning(zsock_print,TAG,"ERROR Checksum test failed for id %i\n",buffer[LISA_INDEX_MSG_ID]);
                }
            }
            else
            {
                send_warning(zsock_print,TAG,"ERROR wrong SENDER ID %i\n",buffer[LISA_INDEX_SENDER_ID]);
            }
        }


        //Get piksi messages
        process_messages();





        //********************************************
        // SENDING IMU DATA to Controller
        //********************************************
        //If we have all IMU messages we send data to controller
        if ((sensors.mag  != NULL) && (sensors.gyro  != NULL)  && (sensors.accel  != NULL))
        {
            //set message type corresponding to the data currently available
            if ((sensors.gps != NULL) && (sensors.airspeed != NULL))
            {
                sensors.type = PROTOBETTY__SENSORS__TYPE__IMU_GPS_AIRSPEED;
            }
            else if (sensors.gps != NULL)
            {
                sensors.type = PROTOBETTY__SENSORS__TYPE__IMU_GPS;
            }
            else if (sensors.airspeed != NULL)
            {
                sensors.type = PROTOBETTY__SENSORS__TYPE__IMU_AIRSPEED;
            }
            else
            {
                sensors.type = PROTOBETTY__SENSORS__TYPE__IMU_ONLY;
            }
            //get size of packed data
            packed_length = protobetty__sensors__get_packed_size(&sensors);
            //pack data to buffer
            protobetty__sensors__pack(&sensors,zmq_buffer);
            //sending sensor message over zmq            
            int zs = zmq_send(zsock_sensors, zmq_buffer, packed_length, 0);
            if (zs < 0) {
                txfails++;
            } else {
                send_debug(zsock_print,TAG,"IMU sent to controller!, size: %u\n", packed_length);
            }
            log_data.sensors = &sensors;
            //get size of packed data
            packed_length = protobetty__log_message__get_packed_size(&log_data);
            //pack data to buffer
            protobetty__log_message__pack(&log_data,zmq_buffer);
            zs = zmq_send(zsock_log, zmq_buffer, packed_length, ZMQ_NOBLOCK);
            if (zs < 0) {
                txfails++;
            } else {
                send_debug(zsock_print,TAG,"IMU sent to controller!, size: %u\n", packed_length);
            }
            //Resetting
            if (sensors.type == PROTOBETTY__SENSORS__TYPE__IMU_GPS
                    ||sensors.type == PROTOBETTY__SENSORS__TYPE__IMU_GPS_AIRSPEED )
            {
                gps.position = NULL;
                gps.velocity = NULL;
            }
            protobetty__sensors__init(&sensors);
            protobetty__log_message__init(&log_data);
        }
    }

    /* Shouldn't get here. */
    return 0;
}



/*
 * Callback functions to interpret SBP messages.
 * Every message ID has a callback associated with it to
 * receive and interpret the message payload.
 */
void piksi_pos_llh_callback(u_int16_t sender_id __attribute__((unused)), u_int8_t len __attribute__((unused)), u8 msg[], void *context __attribute__((unused)))
{
    /* Structs that messages from Piksi will feed. */
    static piksi_position_llh_t pos_llh;
    static Protobetty__GpsLLH gps_llh = PROTOBETTY__GPS_LLH__INIT;
    static Protobetty__GpsData gps_pos = PROTOBETTY__GPS_DATA__INIT;
    static Protobetty__Timestamp gps_time = PROTOBETTY__TIMESTAMP__INIT;
    static Protobetty__Xyz xyz = PROTOBETTY__XYZ__INIT;
    pos_llh = *(piksi_position_llh_t *)msg;
    gps_llh.position = &gps_pos;
    gps_llh.position->time = pos_llh.tow;
    gps_llh.position->n_satellites = pos_llh.n_sats;
    gps_llh.position->h_accuracy = pos_llh.h_accuracy;
    gps_llh.position->v_accuracy = pos_llh.v_accuracy;
    xyz.x = pos_llh.lat;
    xyz.y = pos_llh.lon;
    xyz.z = pos_llh.height;
    gps_llh.position->data = &xyz;
    get_protbetty_timestamp(&gps_time);
    gps_llh.timestamp = &gps_time;
    log_data.gps_llh = &gps_llh;
    printf("pos_llh: lat: %f, long: %f, height: %f \n", pos_llh.lat, pos_llh.lon, pos_llh.height);
}
void piksi_heartbeat_callback(u_int16_t sender_id __attribute__((unused)), u_int8_t len __attribute__((unused)), u8 msg[], void *context __attribute__((unused)))
{
    static piksi_heartbeat_t heartbeat;
    heartbeat = *(piksi_heartbeat_t *)msg;
    printf("heartbeat: %d\n", heartbeat.flags);
}
void piksi_baseline_ned_callback(u_int16_t sender_id __attribute__((unused)), u_int8_t len __attribute__((unused)), u8 msg[], void *context __attribute__((unused)))
{
    static piksi_baseline_ned_t baseline_ned;
    static Protobetty__GpsData gps_pos = PROTOBETTY__GPS_DATA__INIT;
    static Protobetty__Xyz xyz = PROTOBETTY__XYZ__INIT;
    baseline_ned = *(piksi_baseline_ned_t *)msg;
    send_debug(zsock_print,TAG,"Received PIKSI baseline NED (%u ms) N:%i,E:%i,D:%i",
               baseline_ned.tow,
               baseline_ned.n,
               baseline_ned.e,
               baseline_ned.d);
    gps_pos.h_accuracy = baseline_ned.h_accuracy * 1e-3;
    gps_pos.v_accuracy = baseline_ned.v_accuracy * 1e-3;
    gps_pos.n_satellites= baseline_ned.n_sats;
    gps_pos.time = baseline_ned.tow;
    xyz.x = (double)baseline_ned.n* 1e-3;
    xyz.y = (double)baseline_ned.e* 1e-3;
    xyz.z = (double)baseline_ned.d* 1e-3;
    gps_pos.data = &xyz;
    gps.position = &gps_pos;
    complete_gps_message();
}
void piksi_vel_ned_callback(u_int16_t sender_id __attribute__((unused)), u_int8_t len __attribute__((unused)), u8 msg[], void *context __attribute__((unused)))
{
    static piksi_velocity_ned_t vel_ned;
    static Protobetty__GpsData gps_vel = PROTOBETTY__GPS_DATA__INIT;
    static Protobetty__Xyz xyz = PROTOBETTY__XYZ__INIT;
    vel_ned = *(piksi_velocity_ned_t *)msg;
    send_debug(zsock_print,TAG,"Received PIKSI velocity NED (%u ms) N:%i,E:%i,D:%i",
               vel_ned.tow,
               vel_ned.n,
               vel_ned.e,
               vel_ned.d);
    gps_vel.h_accuracy = vel_ned.h_accuracy;
    gps_vel.v_accuracy = vel_ned.v_accuracy;
    gps_vel.n_satellites= vel_ned.n_sats;
    gps_vel.time = vel_ned.tow;
    xyz.x = (double)vel_ned.n* 1e-3;
    xyz.y = (double)vel_ned.e* 1e-3;
    xyz.z = (double)vel_ned.d* 1e-3;
    gps_vel.data = &xyz;
    gps.velocity = &gps_vel;
    complete_gps_message();
}
void piksi_dops_callback(u_int16_t sender_id __attribute__((unused)), u_int8_t len __attribute__((unused)), u8 msg[], void *context __attribute__((unused)))
{
    static piksi_dops_t dops;
    dops = *(piksi_dops_t *)msg;
    printf("dops tow: %u \n", dops.tow);
}
void piksi_gps_time_callback(u_int16_t sender_id __attribute__((unused)), u_int8_t len __attribute__((unused)), u8 msg[], void *context __attribute__((unused)))
{
    static piksi_time_t gps_time;
    gps_time = *(piksi_time_t *)msg;
    printf("time (%d, %d, %d)\n", gps_time.wn, gps_time.tow, gps_time.ns);
}

void complete_gps_message(void)
{
    if (gps.velocity != NULL && gps.position != NULL)
    {
        get_protbetty_timestamp(gps.timestamp);
        sensors.gps = &gps;
    }
}










