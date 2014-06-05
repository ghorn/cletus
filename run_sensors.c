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
#include "./sensors.h"
#include "./misc.h"



static FILE *open_sensor_file(const char *path) {
  FILE *butts = fopen(path, "r");
  if (NULL == butts)
    err("couldn't open file '%s' for reading!", path);
  return butts;
}

/* ZMQ resources */
static void *zctx = NULL;
static void *zsock_out = NULL;
static void *zsock_log = NULL;

/* Error tracking. */
int txfails = 0, rxfails = 0;

static void __attribute__((noreturn)) die(int code) {
  zdestroy(zsock_log, NULL);
  zdestroy(zsock_out, zctx);
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
  int errRet;
  //Communication setup
  uint8_t input_buffer[INPUT_BUFFER_SIZE];
  //init the data decode pointers
  init_decoding();
  errRet = serial_port_setup();
  if(errRet!=UART_ERR_NONE){
      err("couldn't initialize serial port");
    }




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

  /* Set a low high-water mark (a short queue length, measured in
   * messages) so that a sending PUSH will block if the receiving PULL
   * isn't reading.  In the case of PUB/SUB, we still want a short
   * queue; it will prevent outdated messages from building up.  We
   * also set a small-ish buffer size so that the PUSH/PULL socket
   * pair will block or a PUB/SUB socket pair won't accumulate too
   * many outdated messages. */
  zsock_out = setup_zmq_sender(SENSORS_CHAN, &zctx, ZMQ_PUSH, 2, 500);
  if (NULL == zsock_out)
    return 1;
  /* Use big buffers here.  We're just publishing the data for
   * logging, so we don't mind saving some data until the logger can
   * receive it. */
  zsock_log = setup_zmq_sender(LOG_CHAN, &zctx, ZMQ_PUB, 1000, 100000);
  if (NULL == zsock_log)
    die(1);

  /* Sensor input. */
  FILE *serial = open_sensor_file("/dev/zero");
  if (NULL == serial)
    die(1);
  const int serialfd = fileno(serial);
  if (-1 == serialfd) {
      err("couldn't get file descriptor: %s", strerror(errno));
      die(1);
    }

  /* Sensor data storage. */
  sensors_t outgoing;

  zmq_pollitem_t polls[] = {
    /* Outputs (just our socket to send data to the controller and the
               * logger socket) */
    { .socket = zsock_out,
      .fd = -1,
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


  zmq_pollitem_t *outputs = &polls[0];
  const int npolls = sizeof(polls) / sizeof(polls[0]);
  /* const int noutputs = npolls - ninputs; */

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
      usleep(5000); // 200 Hz
      if (get_lisa_data(&outgoing, input_buffer))
        {
          if (outgoing.accel.updated)
            {
              printf("Received Acceleration data (X:%i ; Y:%i ; Z:%i\n",
                     outgoing.accel.data.x,outgoing.accel.data.y,outgoing.accel.data.z);
            }
          if (outgoing.gps_vel.updated)
            {
              printf("Received GPS Velocity data (X:%i ; Y:%i ; Z:%i\n",
                     outgoing.gps_vel.data.x,outgoing.gps_vel.data.y,outgoing.gps_vel.data.z);
            }
          if (outgoing.gps_pos.updated)
            {
              printf("Received GPS Position data (X:%i ; Y:%i ; Z:%i\n",
                     outgoing.gps_pos.data.x,outgoing.gps_pos.data.y,outgoing.gps_pos.data.z);
            }
          if (outgoing.gyro.updated)
            {
              printf("Received Gyro data (X:%i ; Y:%i ; Z:%i\n",
                     outgoing.gyro.data.x,outgoing.gyro.data.y,outgoing.gyro.data.z);
            }
          outputs[0].events = outputs[1].events = ZMQ_POLLOUT;
        }


      if (bail) die(bail);
      if (outputs[0].revents & ZMQ_POLLOUT) {
          /* In here is the stuff specific to what this socket does; as
       * before, this might get broken out if you were to report
       * different sensor values to more than one place. */
          const void *bufs[] = {&outgoing};
          const uint32_t lens[] = {sizeof(outgoing)};
          const int zs = zmq_sendm(zsock_out, bufs, lens,
                                   sizeof(lens) / sizeof(lens[0]));
          if (zs < 0) {
              txfails++;
            } else {
              printf("Sent to controller!, size: %d\n", (int)sizeof(sensors_t));
              /* Clear the events flag so we won't try to send until we
         * have more data. */
              outputs[0].events = 0;
            }
          outputs[0].revents = 0;
        }

      if (bail) die(bail);
      if (outputs[1].revents & ZMQ_POLLOUT) {
          const uint8_t type = LOG_MESSAGE_SENSORS;
          const void *bufs[] = {&type, &outgoing};
          const uint32_t lens[] = {sizeof(type), sizeof(outgoing)};
          const int zs = zmq_sendm(zsock_log, bufs, lens,
                                   sizeof(lens) / sizeof(lens[0]));
          if (zs < 0) {
              txfails++;
            } else {
              printf("Sent to logger!\n");
              /* Clear the events flag so we won't try to send until we
         * have more data. */
              outputs[1].events = 0;
            }
          outputs[1].revents = 0;
        }
    }

  /* Shouldn't get here. */
  return 0;
}
