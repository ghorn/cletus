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

#include "lisa_communication/data_decoding.h"
#include "lisa_communication/uart_communication.h"


static FILE *open_actuator_file(const char *path) {
  FILE *butts = fopen(path, "r");
  if (NULL == butts)
    err("couldn't open file '%s' for reading!", path);
  return butts;
}

/* ZMQ resources */
static void *zctx = NULL;
static void *zsock_in = NULL;
static void *zsock_log = NULL;

/* Error tracking. */
int txfails = 0, rxfails = 0;

static void __attribute__((noreturn)) die(int code) {
  zdestroy(zsock_log, NULL);
  zdestroy(zsock_in, zctx);
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
  const int rt_interval = RT_INTERVAL;


  //init the data decode pointers
  init_decoding();
  int errRet = serial_port_setup();
  if(errRet!=UART_ERR_NONE){
      err("couldn't initialize serial port");
    }


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
  /* Use big buffers here.  We're just publishing the data for
   * logging, so we don't mind saving some data until the logger can
   * receive it. */
  zsock_log = setup_zmq_sender(LOG_CHAN, &zctx, ZMQ_PUB, 1000, 100000);
  if (NULL == zsock_log)
    die(1);

  /* Sensor input. */
  FILE *serial = open_actuator_file("/dev/null");
  if (NULL == serial)
    die(1);
  const int serialfd = fileno(serial);
  if (-1 == serialfd) {
      err("couldn't get file descriptor: %s", strerror(errno));
      die(1);
    }

  /* Actuator data storage. */
  actuators_t incoming;
  lisa_message_t output;
  output.startbyte = 0x99;
  output.length = sizeof(lisa_message_t);
  output.sender_id = BONE_PLANE;
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
    },
    {
      .socket = zsock_log,
      .fd = -1,
      .events = 0,
      .revents = 0
    }
  };


  zmq_pollitem_t* poll_controller = &polls[0];
  zmq_pollitem_t* poll_log = &polls[1];

  const int npolls = sizeof(polls) / sizeof(polls[0]);


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
          const int zr = zmq_recvm(zsock_in, (uint8_t *) &incoming,
                                   sizeof(incoming));
          if (zr < (int) sizeof(incoming)) {
              err("couldn't read actuator commands!");
              rxfails++;
              /* Better clear the output flag, in case we corrupted the
         * data. */
              poll_log->events = 0;
            } else {
              printf("read from controller OK!, time: %.3f\n",floating_time(&(incoming.stop)));
              /* Data OK -- enable output sockets. */
              convert_for_lisa(&incoming, &output);
              serial_port_write((uint8_t*)&output,sizeof(output));
#ifdef DEBUG
              printf("Writing servo commannds 1 -> %i \n\t 2 -> %i\n\t 3 -> %i\n\t 4 -> %i\n\t 5 -> %i\n\t 6 -> %i\n\t 7 -> %i\n",
                     (int)output.servos_msg.servo_1, (int)output.servos_msg.servo_2, (int)output.servos_msg.servo_3,
                     (int)output.servos_msg.servo_4, (int)output.servos_msg.servo_5, (int)output.servos_msg.servo_6,
                     (int)output.servos_msg.servo_7);
#endif
              poll_log->events = ZMQ_POLLOUT;
            }
          /* Clear the poll state. */
          poll_controller->revents = 0;
        }


      if (bail) die(bail);
      if (poll_log->revents & ZMQ_POLLOUT) {
          const uint8_t type = LOG_MESSAGE_SENSORS;
          const void *bufs[] = {&type, &incoming};
          const uint32_t lens[] = {sizeof(type), sizeof(incoming)};
          const int zs = zmq_sendm(zsock_log, bufs, lens,
                                   sizeof(lens) / sizeof(lens[0]));
          if (zs < 0) {
              txfails++;
            } else {
              printf("Sent to logger!\n");
              poll_log->events = 0;
            }
          poll_log->revents = 0;
        }

      calc_next_shot(&t,rt_interval);

    }

  /* Shouldn't get here. */
  return 0;
}
