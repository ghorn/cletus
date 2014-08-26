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
#include <stdio.h>
#include <sys/types.h>
#include <sys/mman.h>

#include "./zmq.h"
#include "./comms.h"
#include "./structures.h"
#include "./log.h"
#include "./misc.h"
#include "./print_output.h"

#include "./controller.h"
#include "./protos_c/messages.pb-c.h"



#define MAX_MSG_SIZE 512
long safe_to_file(const char* const memory,uint64_t nitems);


/* ZMQ resources */
static void *zctx = NULL;
static void *zsock_sensors = NULL;
static void *zsock_actuators = NULL;
void *zsock_print = NULL;
char *ptr_temp_memory;
static uint64_t counter = 0;
static uint64_t NUMBER_OF_LOGS = 0;
char* TAG = "RUN_LOGGER";


/* Error tracking. */
int txfails = 0, rxfails = 0;

static void __attribute__((noreturn)) die(int code) {

    send_info(zsock_print,TAG,"Starting Logging now....");
    safe_to_file(ptr_temp_memory, counter);
    send_info(zsock_print,TAG,"Finished Logging");

    free_workbuf(ptr_temp_memory, NUMBER_OF_LOGS*PROTOBETTY__MESSAGE__CONSTANTS__MAX_MESSAGE_SIZE);
    zdestroy(zsock_print, NULL);
    zdestroy(zsock_actuators, NULL);
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
    set_priority(&param, 10);
    stack_prefault();
    struct timespec t;
    const int rt_interval = 1000000000;

    NUMBER_OF_LOGS = 1000000;




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


    zsock_sensors = setup_zmq_receiver(SENSORS_CHAN, &zctx, ZMQ_SUB, NULL, 1000, 500);
    if (NULL == zsock_sensors)
        return 1;
    zsock_actuators = setup_zmq_sender(ACTUATORS_CHAN, &zctx, ZMQ_PUB, 1, 500);
    if (NULL == zsock_actuators)
        die(1);
    zsock_print = setup_zmq_sender(PRINT_CHAN, &zctx, ZMQ_PUSH, 100, 500);
    if (NULL == zsock_print)
        die(1);




    zmq_pollitem_t polls[] = {

        {
            .socket=zsock_sensors,
            .fd=-1,
            .events= ZMQ_POLLIN,
            .revents=0
        }
    };

    //Pollitem for sensors
    zmq_pollitem_t* poll_sensors = &polls[0];



    ptr_temp_memory = alloc_workbuf(NUMBER_OF_LOGS*PROTOBETTY__MESSAGE__CONSTANTS__MAX_MESSAGE_SIZE);



    const u_int8_t npolls = sizeof(polls) / sizeof(polls[0]);

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
        const int polled = zmq_poll(polls, npolls, 1000);
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
            const int zmq_received = zmq_recvm(zsock_sensors,
                                               (uint8_t*) &ptr_temp_memory[counter*PROTOBETTY__MESSAGE__CONSTANTS__MAX_MESSAGE_SIZE],
                    PROTOBETTY__MESSAGE__CONSTANTS__MAX_MESSAGE_SIZE);
            if (zmq_received > 0)
                counter++;
            /* Clear the poll state. */
            poll_sensors->revents = 0;
        }


        calc_next_shot(&t,rt_interval);

    }

    /* Shouldn't get here. */
    return 0;
}

long safe_to_file(const char* const memory,uint64_t nitems)
{
    FILE *ptr_myfile;
    //Open file
    timestamp_t timestamp;
    gettime(&timestamp);
    char filename[50];
    snprintf(filename, sizeof(filename),"%lu_logdata.bin",timestamp.tsec);
    ptr_myfile=fopen(filename,"wb");
    if (!ptr_myfile)
    {
        printf("Unable to open file!");
        return -1;
    }
    //Allocate memory for received sensor data
    Protobetty__LogSensors log = PROTOBETTY__LOG_SENSORS__INIT;
    Protobetty__Sensors *sensors[nitems];
    //Put sensor data into log message
    for (uint64_t i = 0; i < nitems; i++)
    {
        sensors[i] = protobetty__sensors__unpack(NULL,
                                                 PROTOBETTY__MESSAGE__CONSTANTS__MAX_MESSAGE_SIZE,
                                                 (uint8_t*)&memory[i*PROTOBETTY__MESSAGE__CONSTANTS__MAX_MESSAGE_SIZE]);
        send_info(zsock_print,TAG,"Item %llu of %llu", i, nitems);
        printf("Item %lu of %lu", i, nitems);

    }
    //Set number of items in log message
    log.n_data = nitems;
    //set data
    log.data = sensors;
    // back it to buffer
    const uint64_t packed_size = protobetty__log_sensors__get_packed_size(&log);
    uint8_t* buffer = alloc_workbuf(packed_size);
    protobetty__log_sensors__pack(&log,buffer);
    //write bytewise data to file
    send_debug(zsock_print, TAG, "Writing data to file ...\n");
    printf("Writing data to file ...");

    for (uint64_t i = 0; i < packed_size; i++)
    {
        fwrite(&buffer[i], sizeof(uint8_t), 1, ptr_myfile);
        printf(".");
    }
    send_debug(zsock_print, TAG, "Finsihed writing. Closing File.");
    printf("Finsihed writing. Closing File.\n");
    free_workbuf(buffer,packed_size);

    fclose(ptr_myfile);

    return packed_size;

}


