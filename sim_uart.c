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
#include <sys/ioctl.h>

#include "./zmq.h"
#include "./comms.h"
#include "./structures.h"
#include "./log.h"
#include "./misc.h"

#include "./uart.h"
#include "./lisa_messages.h"
#include "./print_output.h"


#ifdef ALL
#define IMU
#define RC
#define AHRS
#define AIRSPEED
#define GPS
#endif

/* ZMQ resources */
static void *zctx = NULL;
static void *zsock_uart = NULL;
static void *zsock_gyro = NULL;
static void *zsock_mag = NULL;
static void *zsock_accel = NULL;
static void *zsock_airspeed = NULL;
static void *zsock_lisa = NULL;
static void *zsock_print = NULL;

static void *zsock_log = NULL;

/* Error tracking. */
int txfails = 0, rxfails = 0;
char* TAG ="SIM_UART";

static void __attribute__((noreturn)) die(int code) {
    zdestroy(zsock_uart, NULL);
    zdestroy(zsock_log, NULL);
    zdestroy(zsock_lisa, NULL);

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



    const int SLEEP_TIME_us = 10;

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

    zsock_lisa = setup_zmq_sender(LISA_CHAN, &zctx, ZMQ_PUB, 5, 500);
    if (NULL == zsock_lisa)
        die(1);
    zsock_log = setup_zmq_sender(LOG_CHAN, &zctx, ZMQ_PUB, 1000, 100000);
    if (NULL == zsock_log)
        die(1);
    zsock_accel = setup_zmq_sender(IMU_ACCEL_CHAN, &zctx, ZMQ_PUB, 1, 500);
    if (NULL == zsock_lisa)
        die(1);
    zsock_gyro = setup_zmq_sender(IMU_GYRO_CHAN, &zctx, ZMQ_PUB, 1, 500);
    if (NULL == zsock_lisa)
        die(1);
    zsock_mag = setup_zmq_sender(IMU_MAG_CHAN, &zctx, ZMQ_PUB, 1, 500);
    if (NULL == zsock_lisa)
        die(1);
    zsock_airspeed = setup_zmq_sender(AIRSPEED_CHAN, &zctx, ZMQ_PUB, 5, 500);
    if (NULL == zsock_lisa)
        die(1);
    zsock_print = setup_zmq_sender(PRINT_CHAN, &zctx, ZMQ_PUB, 100, 500);
    if (NULL == zsock_print)
        die(1);




    unsigned char msg_data[INPUT_BUFFER_SIZE];



    //Allocate messages
    accel_raw_t accel_dummy;
    mag_raw_t mag_dummy;
    gyro_raw_t gyro_dummy;
    //Assigning IDs
    accel_dummy.id = IMU_ACCEL_SCALED;
    mag_dummy.id = IMU_MAG_SCALED;
    gyro_dummy.id = IMU_GYRO_SCALED;


    uint8_t sequenceNumber = 0;




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

        accel_dummy.id = sequenceNumber;
        accel_dummy.data.x = rand();
        accel_dummy.data.y = rand();
        accel_dummy.data.z = rand();

        mag_dummy.id = sequenceNumber;
        mag_dummy.data.x = rand();
        mag_dummy.data.y = rand();
        mag_dummy.data.z = rand();

        gyro_dummy.id = sequenceNumber;
        gyro_dummy.data.x = rand();
        gyro_dummy.data.y = rand();
        gyro_dummy.data.z = rand();

        sequenceNumber++;
        timestamp_t timestamp;
        gettime(&timestamp);
        accel_dummy.timestamp = timestamp;
        gyro_dummy.timestamp = timestamp;
        mag_dummy.timestamp = timestamp;


        memcpy(&msg_data[0], &accel_dummy,sizeof(accel_raw_t));
        int returned = zmq_send(zsock_accel,&msg_data[0],sizeof(accel_raw_t),ZMQ_NOBLOCK);

        memcpy(&msg_data[0], &mag_dummy, sizeof(mag_raw_t));
        returned = zmq_send(zsock_mag,&msg_data[0],sizeof(mag_raw_t),ZMQ_NOBLOCK);


        memcpy(&msg_data[0], &gyro_dummy,sizeof(gyro_raw_t));
        returned =  zmq_send(zsock_gyro,&msg_data[0],sizeof(gyro_raw_t),ZMQ_NOBLOCK);
        send_debug(zsock_print,TAG,"Sending GYRO, MAG, ACCEL with ID: %u [Bytes:%u] ",sequenceNumber, returned);


        //sleep
        usleep(SLEEP_TIME_us);
    }

    /* Shouldn't get here. */
    return 0;

}


