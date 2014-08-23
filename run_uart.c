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

#include "./uart.h"
#include "./lisa_messages.h"




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

static char* TAG = "RUN_UART";

void *zsock_print = NULL;
static void *zsock_log = NULL;

/* Error tracking. */
int txfails = 0, rxfails = 0;

static void __attribute__((noreturn)) die(int code) {
    zdestroy(zsock_uart, NULL);
    zdestroy(zsock_log, NULL);
    zdestroy(zsock_lisa, NULL);

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

    zsock_print = NULL;

    struct sched_param param;
    set_priority(&param, 49);
    stack_prefault();

    int err = serial_port_setup();
    if (err != UART_ERR_NONE)
        printf("Error setting up UART \n");

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

    zsock_lisa = setup_zmq_sender(LISA_CHAN, &zctx, ZMQ_PUB, 5, 500);
    if (NULL == zsock_lisa)
        die(1);
    zsock_log = setup_zmq_sender(LOG_CHAN, &zctx, ZMQ_PUB, 1000, 100000);
    if (NULL == zsock_log)
        die(1);
    zsock_accel = setup_zmq_sender(IMU_ACCEL_CHAN, &zctx, ZMQ_PUB, 5, 500);
    if (NULL == zsock_lisa)
        die(1);
    zsock_gyro = setup_zmq_sender(IMU_GYRO_CHAN, &zctx, ZMQ_PUB, 5, 500);
    if (NULL == zsock_lisa)
        die(1);
    zsock_mag = setup_zmq_sender(IMU_MAG_CHAN, &zctx, ZMQ_PUB, 5, 500);
    if (NULL == zsock_lisa)
        die(1);
    zsock_airspeed = setup_zmq_sender(AIRSPEED_CHAN, &zctx, ZMQ_PUB, 5, 500);
    if (NULL == zsock_lisa)
        die(1);
    zsock_print = setup_zmq_sender(PRINT_CHAN, &zctx, ZMQ_PUB, 5, 500);
    if (NULL == zsock_print)
        die(1);


    zmq_pollitem_t polls[] = {

        {
            .socket=NULL,
            .fd=serial_stream->fd,
            .events= ZMQ_POLLIN,
            .revents=0
        },
        {
            .socket=NULL,
            .fd=serial_stream->fd,
            .events= 0,
            .revents=0
        },
        {
            .socket=NULL,
            .fd=serial_stream->fd,
            .events= 0,
            .revents=0
        },
        {
            .socket=zsock_lisa,
            .fd=-1,
            .events=0,
            .revents=0
        },
        {
            .socket = zsock_log,
            .fd = -1,
            .events = 0,
            .revents = 0
        }
    };

    //    zmq_pollitem_t* poll_startbyte = &polls[0];
    //    zmq_pollitem_t* poll_length = &polls[1];
    //    zmq_pollitem_t* poll_message = &polls[2];
    zmq_pollitem_t* poll_lisa = &polls[3];

    //    const int npolls = sizeof(polls) / sizeof(polls[0]);
    //    int msg_length_counter = 0;
    //    uint8_t msg_startbyte;
    int msg_length;
    uint8_t msg_buffer[INPUT_BUFFER_SIZE];


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

        if(find_startbyte(poll_lisa,msg_buffer) > 0)
        {
            msg_length = read_lisa_message(poll_lisa, msg_buffer);
            if (msg_length > 0)
            {
                //Check 1: Sender ID must be correct.
                if (msg_buffer[1] == SENDER_ID)
                {
                    //Check 2: Checksum must be correct
                    if (check_checksum(msg_buffer) == UART_ERR_NONE)
                    {

                        msg_length = add_timestamp(msg_buffer, msg_length);
                        msg_length -= BYTES_HEADER;
#ifdef DEBUG
                        send_debug(zsock_print,TAG,"Passed Checksum test. Sending Message [%i bytes] with ID %i\n",
                                   msg_length, msg_buffer[LISA_INDEX_MSG_ID]);
#endif
                        switch (msg_buffer[LISA_INDEX_MSG_ID]) {
                        case IMU_ACCEL:
                        case IMU_ACCEL_RAW:
                        case IMU_ACCEL_SCALED:
                            zmq_send(zsock_accel,&msg_buffer[LISA_INDEX_MSG_ID],msg_length,0);
                            break;
                        case IMU_GYRO:
                        case IMU_GYRO_RAW:
                        case IMU_GYRO_SCALED:
                            zmq_send(zsock_gyro,&msg_buffer[LISA_INDEX_MSG_ID],msg_length,0);
                            break;
                        case IMU_MAG:
                        case IMU_MAG_RAW:
                        case IMU_MAG_SCALED:
                            zmq_send(zsock_mag,&msg_buffer[LISA_INDEX_MSG_ID],msg_length,0);
                            break;
                        case AIRSPEED_ETS:
                            zmq_send(zsock_airspeed,&msg_buffer[LISA_INDEX_MSG_ID],msg_length,0);
                            break;
                        default:
                            zmq_send(zsock_lisa,&msg_buffer[LISA_INDEX_MSG_ID],msg_length,0);
                            break;
                        }
                        poll_lisa->events = ZMQ_POLLOUT;
                    }
                }
                else{
                    send_debug(zsock_print,TAG,"ERROR wrong SENDER ID %i\n",msg_buffer[0]);
                    serial_port_flush_input();
                }
            }
        }

        //        //******************************************************
        //        /* Poll for activity on UART; time out after 10 milliseconds. */
        //        //******************************************************
        //        const int polled = zmq_poll(polls, npolls, 10);
        //        if (polled < 0) {
        //            if (bail) die(bail);
        //            zerr("while polling");
        //            continue;
        //        } else if (polled == 0) {
        //            if (bail) die(bail);
        //            continue;
        //        }


        //        if (bail) die(bail);
        //        //******************************************************
        //        // Check if we polled an event for startbyte
        //        //******************************************************
        //        if (poll_startbyte->revents & ZMQ_POLLIN) {
        //            //get data from serial port
        //            ioctl(serial_stream->fd, FIONREAD, &msg_length_counter); //set to number of bytes in buffer
        //            if(read_uart(&msg_startbyte,1) ==1)
        //            {
        //                //Check if we found the right startbyte
        //                if (msg_startbyte == LISA_STARTBYTE)
        //                {
        //#ifdef DEBUG
        //                    send_debug(zsock_print,TAG,"Read Lisa startbyte. \n");
        //#endif
        //                    //Deactivate Startbyte poller
        //                    poll_startbyte->events = 0;
        //                    //Activate message length poller
        //                    poll_length->events = ZMQ_POLLIN;
        //                }
        //            }
        //            poll_startbyte->revents = 0;
        //        }
        //        //******************************************************
        //        //Get Message length after reading startbyte
        //        //******************************************************
        //        else if (poll_length->revents & ZMQ_POLLIN) {
        //            ioctl(serial_stream->fd, FIONREAD, &msg_length_counter); //set to number of bytes in buffer
        //            if(read_uart(&msg_length,1) ==1)
        //            {
        //#ifdef DEBUG
        //                send_debug(zsock_print,TAG,"Read Message Length [%i bytes] \n", msg_length);
        //#endif
        //                if (msg_length < 100){
        //                    poll_length->events = 0;
        //                    poll_message->events = ZMQ_POLLIN;
        //                }
        //                else
        //                {
        //                    //Drop message
        //                    poll_length->events = 0;
        //                    poll_startbyte->events = ZMQ_POLLIN;
        //                    msg_length_counter =0;
        //                    serial_port_flush_input();
        //                }
        //            }
        //            poll_length->revents = 0;
        //        }
        //        //******************************************************
        //        //Get Message itself
        //        //******************************************************
        //        else if (poll_message->revents & ZMQ_POLLIN) {
        //            ioctl(serial_stream->fd, FIONREAD, &msg_length_counter); //set to number of bytes in buffer
        //            //Get poll events until message was sent completely
        //            if (msg_length_counter < msg_length)
        //            {
        //                poll_length->revents = 0;
        //                continue;
        //            }
        //            //******************************************************
        //            //Send message without sender ID, so message ID can be used as Filter
        //            //******************************************************
        //            else if(read_uart(msg_data,msg_length-2) == msg_length - 2) //without the already read startbyte and length
        //            {
        //#ifdef DEBUG
        //                send_debug(zsock_print,TAG,"Message was read completely \n");
        //#endif
        //                //Check 1: Sender ID must be correct.
        //                if (msg_data[0] == SENDER_ID)
        //                {
        //                    //Check 2: Checksum must be correct
        //                    if (check_checksum(msg_length,msg_data) == UART_ERR_NONE)
        //                    {

        //                        const int new_length =add_timestamp(&msg_data[1], msg_length);
        //#ifdef DEBUG
        //                        send_debug(zsock_print,TAG,"Passed Checksum test. Sending Message [%i bytes] with ID %i\n",
        //                               new_length, msg_data[1]);
        //#endif
        //                        switch (msg_data[1]) {
        //                        case IMU_ACCEL:
        //                        case IMU_ACCEL_RAW:
        //                        case IMU_ACCEL_SCALED:
        //                            zmq_send(zsock_accel,&msg_data[1],new_length,0);
        //                            break;
        //                        case IMU_GYRO:
        //                        case IMU_GYRO_RAW:
        //                        case IMU_GYRO_SCALED:
        //                            zmq_send(zsock_gyro,&msg_data[1],new_length,0);
        //                            break;
        //                        case IMU_MAG:
        //                        case IMU_MAG_RAW:
        //                        case IMU_MAG_SCALED:
        //                            zmq_send(zsock_mag,&msg_data[1],new_length,0);
        //                            break;
        //                        case AIRSPEED_ETS:
        //                            zmq_send(zsock_airspeed,&msg_data[1],new_length,0);
        //                            break;
        //                        default:
        //                            zmq_send(zsock_lisa,&msg_data[1],new_length,0);
        //                            break;
        //                        }
        //                        poll_lisa->events = ZMQ_POLLOUT;
        //                    }
        //                }
        //                else{
        //                    send_debug(zsock_print,TAG,"ERROR wrong SENDER ID %i\n",msg_data[0]);
        //                    serial_port_flush_input();
        //                }
        //                msg_length_counter = 0;
        //                poll_message->events =0;
        //                poll_startbyte->events= ZMQ_POLLIN;
        //            }
        //            poll_message->revents = 0;
        //        }
    }

    /* Shouldn't get here. */
    return 0;
}

