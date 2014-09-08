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
#include "./actuators.h"

#include "./uart.h"
#include "./lisa_messages.h"


char* const TAG = "RUN_ACTUATORS";

/* ZMQ resources */
static void *zctx = NULL;
static void *zsock_in = NULL;
void *zsock_print = NULL;


/* Error tracking. */
int txfails = 0, rxfails = 0;

static void __attribute__((noreturn)) die(int code) {
    zdestroy(zsock_in, NULL);
    zdestroy(zsock_print,NULL);
    serial_port_close();
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
    printf("%i", argc);
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
        printf("Setting frequency to %li Hz.\n", frequency);
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


    int err = serial_port_setup(NULL);
    if (err != UART_ERR_NONE)
        printf("Error setting up UART \n");

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
    zsock_in = setup_zmq_receiver(ACTUATORS_CHAN, &zctx, ZMQ_SUB, NULL, 1, 500);
    if (NULL == zsock_in)
        return 1;
    zsock_print = setup_zmq_sender(PRINT_CHAN, &zctx, ZMQ_PUSH, 1000, 500);
    if (NULL == zsock_print)
        die(1);



    /* Actuator data storage. */
    Protobetty__Actuators* incoming;
    //create template for actuator message for lisa
    lisa_message_t output;
    output.startbyte = 0x99;
    output.length = sizeof(lisa_message_t);
    output.sender_id = SENDER_ID;
    output.message_id = SERVO_COMMANDS;

#ifdef DEBUG
    printf("Servo Message Header: Startbyte -> %x \n\t length -> %i \n\t SenderID -> %i \n\t MessageID -> %i \n",
           output.startbyte,output.length,output.sender_id,output.message_id);

#endif

    zmq_pollitem_t polls[] = {
        /* Inputs -- in this case, our incoming actuator commands. */
        {
            .socket = zsock_in,
            .fd = -1,
            .events = ZMQ_POLLIN,
            .revents = 0
        }
    };


    zmq_pollitem_t* poll_controller = &polls[0];

    const int npolls = sizeof(polls) / sizeof(polls[0]);
    uint8_t zmq_buffer[PROTOBETTY__MESSAGE__CONSTANTS__MAX_MESSAGE_SIZE]; // Input data container for bytes

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
        if (poll_controller->revents & ZMQ_POLLIN) {
            /* Read in some sensor data from this sensor. */
            const int zr = zmq_recvm(zsock_in, zmq_buffer,
                                     PROTOBETTY__MESSAGE__CONSTANTS__MAX_MESSAGE_SIZE);
            incoming = protobetty__actuators__unpack(NULL,zr,zmq_buffer);


            if (incoming != NULL){
                send_debug(zsock_print,TAG,"Received Actuators message with latency %f ms and \n rudd:%f\n elev:%f\n ail:%f\n flaps:%f",
                           calcCurrentLatencyProto(incoming->timestamp_actuators),
                           incoming->rudd,
                           incoming->elev,
                           incoming->ail,
                           incoming->flaps);
                convert_for_lisa(incoming, &output);
                write_uart((uint8_t*)&output,sizeof(output));
#ifdef DEBUG
                send_debug(zsock_print,TAG,"Writing servo commannds 1 -> %i \n\t 2 -> %i\n\t 3 -> %i\n\t 4 -> %i\n\t 5 -> %i\n\t 6 -> %i\n\t 7 -> %i\n",
                           (int)output.servos_msg.servo_1,
                           (int)output.servos_msg.servo_2,
                           (int)output.servos_msg.servo_3,
                           (int)output.servos_msg.servo_4,
                           (int)output.servos_msg.servo_5,
                           (int)output.servos_msg.servo_6,
                           (int)output.servos_msg.servo_7);
#endif
            }
            /* Clear the poll state. */
            poll_controller->revents = 0;
            poll_controller->events = ZMQ_POLLIN;
        }


        if (bail) die(bail);
        calc_next_shot(&t,rt_interval);

    }

    /* Shouldn't get here. */
    return 0;
}
