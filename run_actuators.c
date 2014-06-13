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


  int  errRet = serial_port_setup();
  if(errRet!=UART_ERR_NONE){
      err("couldn't initialize serial port");
    }


  uint8_t encoded_data[36];

  int i=0;

  while(1)
    {

      Output output;

      //create test data
      output.message.servo_1=-i;
      output.message.servo_2=i;
      output.message.servo_3=i;
      output.message.servo_4=i;
      output.message.servo_5=i;
      output.message.servo_6=i;
      output.message.servo_7=0;
      i=i+2;
      if(i>12800){
          i=0;
        }
      printf("Current Value: %i",i);

      //2. encode the data
      data_encode(output.raw,sizeof(output.raw),encoded_data,SERVER,SERVO_COMMANDS);

      //3. send data over UART
      int new_length = strip_timestamp(encoded_data); //lisa expects a package without a timestamp

      serial_port_write(encoded_data,new_length);

      //usleep(20000); //60 hz

      sleep(1);
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
  zsock_in = setup_zmq_receiver(ACTUATORS_CHAN, &zctx, ZMQ_PULL, NULL, 2, 500);
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

  zmq_pollitem_t polls[] = {
    /* Inputs -- in this case, our incoming actuator commands. */
    { .socket = zsock_in,
      .fd = -1,
      .events = ZMQ_POLLIN,
      .revents = 0
    },
    /* Let's read from stdin as well to make things more fun. */
    { .socket = NULL,
      .fd = fileno(stdin),
      .events = ZMQ_POLLIN,
      .revents = 0
    },
    /* Outputs.  We have a choice: we could simply block on serial
       * writes and not even poll on output. */
    { .socket = NULL,
      .fd = serialfd,
      /* 'events' would be ZMQ_POLLOUT, but we'll wait till we have
           * something to send*/
      .events = 0,
      .revents = 0
    },
    { .socket = zsock_log,
      .fd = -1,
      .events = 0,
      .revents = 0
    }
  };

  const int ninputs = 2;
  zmq_pollitem_t *inputs = &polls[0];
  const int npolls = sizeof(polls) / sizeof(polls[0]);
  /* const int noutputs = npolls - ninputs; */
  zmq_pollitem_t *outputs = &polls[ninputs];

  uint8_t encoded_actuators[36];

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
          /* not sure what to do about it. */
          continue;
        } else if (polled == 0) {
          if (bail) die(bail);
          /* timeout! */
          continue;
        }

      if (bail) die(bail);
      if (inputs[0].revents & ZMQ_POLLIN) {
          /* Read in some sensor data from this sensor. */
          const int zr = zmq_recvm(zsock_in, (uint8_t *) &incoming,
                                   sizeof(incoming));
          if (zr < (int) sizeof(incoming)) {
              err("couldn't read actuator commands!");
              rxfails++;
              /* Better clear the output flag, in case we corrupted the
         * data. */
              outputs[0].events = outputs[1].events = 0;
            } else {
              printf("read from controller OK!, time: %.3f\n",floating_time(&(incoming.stop)));
              /* Data OK -- enable output sockets. */
              outputs[0].events = outputs[1].events = ZMQ_POLLOUT;
            }
          /* Clear the poll state. */
          polls[0].revents = 0;
        }

      if (inputs[1].revents & ZMQ_POLLIN) {
          /* Read in some sensor data from this sensor.  It's stdin, so
       * just do some random thing.*/
          printf("Keyboard cat!\n");
          /* Clear the poll state. */
          polls[1].revents = 0;
        }

      if (bail) die(bail);
      if (outputs[0].revents & ZMQ_POLLOUT) {
          if (fwrite(&incoming, sizeof(incoming), 1, serial) < 1
              && ferror(serial)) {
              txfails++;
            } else {
              printf("Sent to actuators!\n");
              /* Clear the events flag so we won't try to send until we
         * have more data. */
              actuators_t new_data;
              int new_length = set_actuators(&new_data,encoded_actuators);
              serial_port_write(encoded_actuators,new_length);
              outputs[0].events = 0;
            }
          outputs[0].revents = 0;
        }

      if (bail) die(bail);
      if (outputs[1].revents & ZMQ_POLLOUT) {
          const uint8_t type = LOG_MESSAGE_SENSORS;
          const void *bufs[] = {&type, &incoming};
          const uint32_t lens[] = {sizeof(type), sizeof(incoming)};
          const int zs = zmq_sendm(zsock_log, bufs, lens,
                                   sizeof(lens) / sizeof(lens[0]));
          if (zs < 0) {
              txfails++;
            } else {
              printf("Sent to logger!\n");
              outputs[1].events = 0;
            }
          outputs[1].revents = 0;
        }
    }

  /* Shouldn't get here. */
  return 0;
}
