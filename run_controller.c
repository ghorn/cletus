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
#include "./misc.h"

#include "./controller.h"
#include "./protos_c/messages.pb-c.h"



#define MAX_MSG_SIZE 512


/* ZMQ resources */
static void *zctx = NULL;
static void *zsock_sensors = NULL;
static void *zsock_actuators = NULL;
static void *zsock_log = NULL;
void *zsock_print = NULL;


/* Error tracking. */
int txfails = 0, rxfails = 0;

static void __attribute__((noreturn)) die(int code) {
    zdestroy(zsock_actuators, NULL);
    zdestroy(zsock_log, NULL);
    zdestroy(zsock_sensors, zctx);



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
    const int rt_interval = 100000000;



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
#ifdef PUSHPULL
    zsock_sensors = setup_zmq_receiver(SENSORS_CHAN, &zctx, ZMQ_PULL, NULL, 1, 500);
#else
    zsock_sensors = setup_zmq_receiver(SENSORS_CHAN, &zctx, ZMQ_SUB, NULL, 1, 500);
#endif
    if (NULL == zsock_sensors)
        return 1;
    zsock_actuators = setup_zmq_sender(ACTUATORS_CHAN, &zctx, ZMQ_PUB, 1, 500);
    if (NULL == zsock_actuators)
        die(1);
    zsock_log = setup_zmq_sender(LOG_CHAN, &zctx, ZMQ_PUB, 1000, 100000);
    if (NULL == zsock_log)
        die(1);

    /* Data storage. */
    //  sensors_t y_incoming;
    //  actuators_t u_outgoing;


    zmq_pollitem_t polls[] = {

        {
            .socket=zsock_sensors,
            .fd=-1,
            .events= ZMQ_POLLIN,
            .revents=0
        },
        {
            /* Outputs (our socket to send data to the actuators and the
                                                       * logger socket) */
            .socket = zsock_actuators,
            .fd = -1,
            /* 'events' would be ZMQ_POLLOUT, but we'll wait till we have
                                                           * something to send*/
            .events = 0,
            .revents = 0
        },
        {
            .socket = zsock_log,
            .fd = -1,
            .events = 0,
            .revents = 0
        }
    };

    //Pollitem for sensors
    zmq_pollitem_t* poll_sensors = &polls[0];
    //Pollitem for actuators
    zmq_pollitem_t* poll_actuators = &polls[1];
    //Pollitem flor logging
    zmq_pollitem_t* poll_log = &polls[2];


    uint8_t* zmq_buffer = alloc_workbuf(PROTOBETTY__MESSAGE__CONSTANTS__MAX_MESSAGE_SIZE); // Input data container for bytes

    //Placeholders for PROTOBUF data
    Protobetty__Sensors *sensors_ptr;         // Sensors
    Protobetty__Actuators actuators = PROTOBETTY__ACTUATORS__INIT;
    Protobetty__Timestamp actuators_timstamp_start = PROTOBETTY__TIMESTAMP__INIT;
//    TimestampProto actuators_timstamp_stop = TIMESTAMP_PROTO__INIT;
    actuators.start = &actuators_timstamp_start;
//    actuators.stop = &actuators_timstamp_stop;



    const u_int8_t npolls = sizeof(polls) / sizeof(polls[0]);
    unsigned int packed_length;

    clock_gettime(CLOCK_MONOTONIC ,&t);
    /* start after one second */
    t.tv_sec++;

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
        if (bail) die(bail);

        /* wait until next shot */
        clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME, &t, NULL);
        /* Poll for activity; time out after 10 milliseconds. */
        const int polled = zmq_poll(polls, npolls, 10);
        if (polled < 0) {
            if (bail) die(bail);
            zerr("while polling");
            calc_next_shot(&t,rt_interval);
            continue;
        } else if (polled == 0) {
            if (bail) die(bail);
            calc_next_shot(&t,rt_interval);
            continue;
        }



        if (bail) die(bail);
        if (poll_sensors->revents & ZMQ_POLLIN) {
            const int zmq_received = zmq_recvm(zsock_sensors, zmq_buffer,
                                     PROTOBETTY__MESSAGE__CONSTANTS__MAX_MESSAGE_SIZE);

                sensors_ptr = protobetty__sensors__unpack(NULL, zmq_received, zmq_buffer);
                if (sensors_ptr != NULL)
                {
#ifdef DEBUG
                printf("Controller received Sensor data containing: ");
                switch (sensors_ptr->type) {
                case PROTOBETTY__SENSORS__TYPE__IMU_ONLY:
                    printf("IMU_ONLY\n");
                    break;
                case PROTOBETTY__SENSORS__TYPE__IMU_GPS:
                    printf("IMU ; GPS\n");
                    break;
                case PROTOBETTY__SENSORS__TYPE__IMU_AIRSPEED:
                    printf("IMU ; AIRSPEED\n");
                    break;
                case PROTOBETTY__SENSORS__TYPE__IMU_GPS_AIRSPEED:
                    printf("IMU ; GPS ; AIRSPEED\n");
                    break;
                case _PROTOBETTY__SENSORS__TYPE_IS_INT_SIZE:
                    printf("INT SIZE case -> no vailid type\n");
                    break;
                default:
                    printf("UNKNOWN TYPE\n");
                    break;
                }
#endif
                /* Here is where you might run your controller when you get a
                   * complete set of sensor inputs. */
                run_demo_controller(sensors_ptr, &actuators);
                /* Controller went OK (it had damn well better) -- enable
                   * output sockets. */
#ifdef DEBUG
                printf("New Control values: Rudder -> %f \n\t Flaps -> %f\n\t Ailerons -> %f\n\t Elevator -> %f\n",
                       actuators.rudd, actuators.flaps, actuators.ail, actuators.elev);
#endif
                poll_actuators->events = ZMQ_POLLOUT;
                poll_log->events = ZMQ_POLLOUT;
            }
            /* Clear the poll state. */
            poll_sensors->revents = 0;
        }

        if (bail) die(bail);
        if (poll_actuators->revents & ZMQ_POLLOUT) {
            packed_length = protobetty__actuators__get_packed_size(&actuators); //
            protobetty__actuators__pack(&actuators, zmq_buffer);
            const int zs = zmq_send(zsock_actuators,zmq_buffer, packed_length,0);
            if (zs < 0) {
                txfails++;
            } else {
#ifdef DEBUG
                printf("Sent to actuators!\n");
#endif
                /* Clear the events flag so we won't try to send until we
                   * have more data. */
                poll_actuators->events = 0;
                /* calculate next shot */

            }
            poll_actuators->revents = 0;
        }
        calc_next_shot(&t,rt_interval);


        /* I skipped logging; I think you know what to do. */
    }

    /* Shouldn't get here. */
    return 0;
}
