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
#include "./print_output.h"
#include "./protos_c/messages.pb-c.h"



#define MAX_MSG_SIZE 512


/* ZMQ resources */
static void *zctx = NULL;
void *zsock_print = NULL;

/* Error tracking. */
int txfails = 0, rxfails = 0;

static void __attribute__((noreturn)) die(int code) {
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
    set_priority(&param, 10);
    stack_prefault();
    struct timespec t;
    const int rt_interval = 20000;



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

    zsock_print = setup_zmq_receiver(PRINT_CHAN, &zctx, ZMQ_SUB,NULL, 10000, 500);
    if (NULL == zsock_print)
        die(1);


    zmq_pollitem_t polls[] = {

        {
            .socket=zsock_print,
            .fd=-1,
            .events= ZMQ_POLLIN,
            .revents=0
        }
    };

    //Pollitem for print
    zmq_pollitem_t* poll_print = &polls[0];


    uint8_t zmq_buffer[PROTOBETTY__MESSAGE__CONSTANTS__MAX_MESSAGE_SIZE]; // Input data container for bytes

    //Placeholders for PROTOBUF data
    Protobetty__Printf *print_msg_ptr;         // Message




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
        if (poll_print->revents & ZMQ_POLLIN) {
            const int zmq_received = zmq_recvm(zsock_print, zmq_buffer,
                                               PROTOBETTY__MESSAGE__CONSTANTS__MAX_MESSAGE_SIZE);

            print_msg_ptr = protobetty__printf__unpack(NULL, zmq_received, zmq_buffer);
            if (print_msg_ptr != NULL)
            {
                switch (print_msg_ptr->type)
                {
                case PROTOBETTY__PRINTF__TYPE__DEBUG:
                    printf("[%f %s]: %s \n",floating_ProtoTime(print_msg_ptr->timestamp),print_msg_ptr->tag,print_msg_ptr->message);
                    break;
                case PROTOBETTY__PRINTF__TYPE__INFO:
                    printf("[%f %s]: ",floating_ProtoTime(print_msg_ptr->timestamp),print_msg_ptr->tag);
                    LOG_INFO(print_msg_ptr->message);
                    printf("\n");
                    break;
                case PROTOBETTY__PRINTF__TYPE__WARNING:
                    printf("[%f %s]:",floating_ProtoTime(print_msg_ptr->timestamp),print_msg_ptr->tag);
                    LOG_WARNING(print_msg_ptr->message);
                    printf("\n");
                    break;
                case PROTOBETTY__PRINTF__TYPE__ERROR:
                    printf("[%f %s]: ",floating_ProtoTime(print_msg_ptr->timestamp),print_msg_ptr->tag);
                    LOG_ERROR(print_msg_ptr->message);
                    printf("\n");
                    break;
                case _PROTOBETTY__PRINTF__TYPE_IS_INT_SIZE:
                    break;
                default:
                    printf("[%f %s]: %s \n",floating_ProtoTime(print_msg_ptr->timestamp),print_msg_ptr->tag,print_msg_ptr->message);
                    break;
                }


            }
            /* Clear the poll state. */
            poll_print->events = ZMQ_POLLIN;
            poll_print->revents = 0;
        }


        calc_next_shot(&t,rt_interval);


        /* I skipped logging; I think you know what to do. */
    }

    /* Shouldn't get here. */
    return 0;
}

