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
static void *zsock_lisa = NULL;
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

  zmq_pollitem_t* poll_startbyte = &polls[0];
  zmq_pollitem_t* poll_length = &polls[1];
  zmq_pollitem_t* poll_message = &polls[2];
  zmq_pollitem_t* poll_lisa = &polls[3];

  const int npolls = sizeof(polls) / sizeof(polls[0]);
  int msg_length_counter = 0;
  uint8_t msg_startbyte;
  uint8_t msg_length;
  unsigned char msg_data[INPUT_BUFFER_SIZE];


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

      //******************************************************
      /* Poll for activity on UART; time out after 10 milliseconds. */
      //******************************************************
      const int polled = zmq_poll(polls, npolls, 5);
      if (polled < 0) {
          if (bail) die(bail);
          zerr("while polling");
          continue;
        } else if (polled == 0) {
          if (bail) die(bail);
          continue;
        }


      if (bail) die(bail);
      //******************************************************
      // Check if we polled an event for startbyte
      //******************************************************
      if (poll_startbyte->revents & ZMQ_POLLIN) {
          //get data from serial port
          ioctl(serial_stream->fd, FIONREAD, &msg_length_counter); //set to number of bytes in buffer
          if(read_uart(&msg_startbyte,1) ==1)
            {
              //Check if we found the right startbyte
              if (msg_startbyte == LISA_STARTBYTE)
                {
#ifdef DEBUG
                  printf("Read Lisa startbyte. \n");
#endif
                  //Deactivate Startbyte poller
                  poll_startbyte->events = 0;
                  //Activate message length poller
                  poll_length->events = ZMQ_POLLIN;
                }
            }
          poll_startbyte->revents = 0;
        }
      //******************************************************
      //Get Message length after reading startbyte
      //******************************************************
      else if (poll_length->revents & ZMQ_POLLIN) {
          ioctl(serial_stream->fd, FIONREAD, &msg_length_counter); //set to number of bytes in buffer
          if(read_uart(&msg_length,1) ==1)
            {
#ifdef DEBUG
              printf("Read Message Length [%i bytes] \n", msg_length);
#endif
              if (msg_length < 100){
                  poll_length->events = 0;
                  poll_message->events = ZMQ_POLLIN;
                }
              else
                {
                  //Drop message
                  poll_length->events = 0;
                  poll_startbyte->events = ZMQ_POLLIN;
                  msg_length_counter =0;
                  serial_port_flush_input();
                }
            }
          poll_length->revents = 0;
        }
      //******************************************************
      //Get Message itself
      //******************************************************
      else if (poll_message->revents & ZMQ_POLLIN) {
          ioctl(serial_stream->fd, FIONREAD, &msg_length_counter); //set to number of bytes in buffer
          //Get poll events until message was sent completely
          if (msg_length_counter < msg_length)
            {
              poll_length->revents = 0;
              continue;
            }
          //******************************************************
          //Send message without sender ID, so message ID can be used as Filter
          //******************************************************
          else if(read_uart(msg_data,msg_length-2) == msg_length - 2)
            {
#ifdef DEBUG
              printf("Message was read completely \n");
#endif
              if (check_checksum(msg_length,msg_data) == UART_ERR_NONE)
                {
#ifdef DEBUG
                  printf("Passed Checksum test. Sending Message [%i bytes] with ID %i\n",
                         msg_length-3, msg_data[1]);
#endif
                  zmq_send(zsock_lisa,&msg_data[1],msg_length-3,0);
                  poll_lisa->events = ZMQ_POLLOUT;
                }
              else{
                  printf("ERROR with Checksum test with ID %i\n",msg_data[1]);
                  serial_port_flush_input();
                }
              msg_length_counter = 0;
              poll_message->events =0;
              poll_startbyte->events= ZMQ_POLLIN;
            }
          poll_message->revents = 0;
        }
    }

  /* Shouldn't get here. */
  return 0;
}

