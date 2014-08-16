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

#include "../zmq.h"
#include "../comms.h"
#include "../structures.h"
#include "../log.h"
#include "../misc.h"

#include "../lisa_messages.h"



const int BUFFER_SIZE = 50;


/* ZMQ resources */
static void *zctx = NULL;
static void *zsock_data = NULL;

FILE *log_file;


/* Error tracking. */
int txfails = 0, rxfails = 0;

static void __attribute__((noreturn)) die(int code) {
    zdestroy(zsock_data, zctx);

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



    //Setting up priority
    struct sched_param param;
    set_priority(&param, 48);
    stack_prefault();
    //Timer variables
    struct timespec timestamp;


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
    //We need just one socket for getting lisa messages
    zsock_data = setup_zmq_receiver(LISA_CHAN, &zctx, ZMQ_SUB, NULL, 1, 500);
    if (NULL == zsock_data)
        die(1);

    //also only one pollitem polling from LISA_CHANNEL
    zmq_pollitem_t polls[] = {
        {
            .socket=zsock_data,
            .fd=-1,
            .events= ZMQ_POLLIN,
            .revents=0
        }
    };
    zmq_pollitem_t* poll_data = &polls[0];
    const int npolls = sizeof(polls) / sizeof(polls[0]);
    unsigned int counter = 0;

    unsigned char *buffer = malloc(BUFFER_SIZE);

    //sensor data
    MSG_Airspeed_ets airspeed;
    MSG_Gyro_scaled gyro;
    MSG_Accel_scaled accel;
    MSG_Mag_scaled mag;


    //file
    log_file = fopen("logile.txt","w+");



    //***********************************************************
    // MAIN LOOP
    //*********************************************************
    printf("Starting Logger: \n\n");
    for (;;) {
        if (bail) die(bail);
        /* Poll for activity; time out after 10 milliseconds. */


        const int polled = zmq_poll(polls, npolls, 5);
        if (polled < 0) {
            if (bail) die(bail);
            zerr("while polling");
            continue;
        } else if (polled == 0) {
            if (bail) die(bail);
            usleep(20);
            continue;
        }

        if (bail) die(bail);
        if (poll_data->revents & ZMQ_POLLIN)
        {
            counter++;
            zmq_recv(zsock_data,buffer,BUFFER_SIZE,0);
            clock_gettime(CLOCK_MONOTONIC, &timestamp);
            switch (buffer[0])
            {
            case AIRSPEED_ETS:
                memcpy(&airspeed, &buffer[1], sizeof(MSG_Airspeed_ets));
                fprintf(log_file,"%u ; %lld.%.9ld ; %i ; %u ; %u ; %f \n",counter, (long long)timestamp.tv_sec, timestamp.tv_nsec,
                        AIRSPEED_ETS, airspeed.adc, airspeed.offset, airspeed.scaled );
                break;
            case IMU_ACCEL_RAW:
            case IMU_ACCEL_SCALED:
                memcpy(&accel, &buffer[1], sizeof(MSG_Accel_scaled));
                fprintf(log_file,"%u ; %lld.%.9ld ; %i ; %i ; %i ; %i \n",counter, (long long)timestamp.tv_sec, timestamp.tv_nsec,
                        buffer[0], accel.ax, accel.ay, accel.az );
                break;
            case IMU_GYRO_RAW:
            case IMU_GYRO_SCALED:
                memcpy(&gyro, &buffer[1], sizeof(MSG_Gyro_scaled));
                fprintf(log_file,"%u ; %lld.%.9ld ; %i ; %i ; %i ; %i \n",counter, (long long)timestamp.tv_sec, timestamp.tv_nsec,
                        buffer[0], gyro.gx, gyro.gy, gyro.gz );
                break;
            case IMU_MAG_RAW:
            case IMU_MAG_SCALED:
                memcpy(&mag, &buffer[1], sizeof(MSG_Mag_scaled));
                fprintf(log_file,"%u ; %lld.%.9ld ; %i ; %i ; %i ; %i \n",counter, (long long)timestamp.tv_sec, timestamp.tv_nsec,
                        buffer[0], mag.mx, mag.my, mag.mz );
                break;
            default:
                fprintf(log_file,"%u ; %lld.%.9ld ; %i",counter, (long long)timestamp.tv_sec, timestamp.tv_nsec,
                        buffer[0] );
            }
            poll_data->revents = 0;


        }


        /* I skipped logging; I think you know what to do. */
    }

    /* Shouldn't get here. */
    return 0;
}

