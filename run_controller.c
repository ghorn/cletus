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


#ifdef ALL
#define IMU
#define RC
#define AHRS
#define AIRSPEED
#define GPS
#endif

/* ZMQ resources */
static void *zctx = NULL;
static void *zsock_imu = NULL;
static void *zsock_gps = NULL;
static void *zsock_ahrs = NULL;
static void *zsock_airspeed = NULL;
static void *zsock_actuators = NULL;
static void *zsock_log = NULL;

/* Error tracking. */
int txfails = 0, rxfails = 0;

static void __attribute__((noreturn)) die(int code) {
  zdestroy(zsock_actuators, NULL);
  zdestroy(zsock_log, NULL);
  zdestroy(zsock_imu, zctx);
  zdestroy(zsock_gps, zctx);
  zdestroy(zsock_ahrs, zctx);
  zdestroy(zsock_airspeed, zctx);



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

  zsock_imu = setup_zmq_receiver(IMU_CHAN, &zctx, ZMQ_SUB, NULL, 1, 500);
  if (NULL == zsock_imu)
    return 1;
  zsock_gps = setup_zmq_receiver(GPS_CHAN, &zctx, ZMQ_SUB, NULL, 1, 500);
  if (NULL == zsock_gps)
    return 1;
  zsock_airspeed = setup_zmq_receiver(AIRSPEED_CHAN, &zctx, ZMQ_SUB, NULL, 1, 500);
  if (NULL == zsock_airspeed)
    return 1;
  zsock_ahrs = setup_zmq_receiver(AHRS_CHAN, &zctx, ZMQ_SUB, NULL, 1, 500);
  if (NULL == zsock_ahrs)
    return 1;
  zsock_actuators = setup_zmq_sender(ACTUATORS_CHAN, &zctx, ZMQ_PUB, 1, 500);
  if (NULL == zsock_actuators)
    die(1);
  zsock_log = setup_zmq_sender(LOG_CHAN, &zctx, ZMQ_PUB, 1000, 100000);
  if (NULL == zsock_log)
    die(1);

  /* Data storage. */
  sensors_t y_incoming;
  actuators_t u_outgoing;


  zmq_pollitem_t polls[] = {

    {
      .socket=zsock_imu,
      .fd=-1,
      .events= ZMQ_POLLIN,
      .revents=0
    },
    {
      .socket=zsock_gps,
      .fd=-1,
      .events= ZMQ_POLLIN,
      .revents=0
    },
    {
      .socket=zsock_ahrs,
      .fd=-1,
      .events= ZMQ_POLLIN,
      .revents=0
    },
    {
      .socket=zsock_airspeed,
      .fd=-1,
      .events= ZMQ_POLLIN,
      .revents=0
    },
    {
      /* Outputs (our socket to send data to the actuators and the
                                               * logger socket) */
      .socket = zsock_actuators,
      .fd = -1,
      /* 'events' would be ZMQ_POLLOUT, but we'll wait till we have
                                                   * something to send*/
      .events = 0,
      .revents = 0
    },
    {
      .socket = zsock_log,
      .fd = -1,
      .events = 0,
      .revents = 0
    }
  };

#ifdef IMU
  zmq_pollitem_t* poll_imu = &polls[0];
#endif
#ifdef GPS
  zmq_pollitem_t* poll_gps = &polls[1];
#endif
#ifdef AHRS
  zmq_pollitem_t* poll_ahrs = &polls[2];
#endif
#ifdef AIRSPEED
  zmq_pollitem_t* poll_airspeed = &polls[3];
#endif
  zmq_pollitem_t* poll_actuators = &polls[4];
  zmq_pollitem_t* poll_log = &polls[5];



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
      if (poll_imu->revents & ZMQ_POLLIN) {
          const int zr = zmq_recvm(zsock_imu, (uint8_t *) &y_incoming.imu,
                                   sizeof(y_incoming.imu));
          if (zr < (int) sizeof(imu_t)) {
              err("couldn't read sensors!");
              rxfails++;
              /* Better clear the output flag, in case we corrupted the
                   * data. */
              poll_actuators->events = 0;
              poll_log->events = 0;
            } else {
#ifdef DEBUG
              printf("read from sensors OK!\n");
#endif
              /* Here is where you might run your controller when you get a
                   * complete set of sensor inputs. */
              run_demo_controller(&y_incoming, &u_outgoing);
              /* Controller went OK (it had damn well better) -- enable
                   * output sockets. */
#ifdef DEBUG
              printf("New Control values: Rudder -> %f \n\t Flaps -> %f\n\t Ailerons -> %f\n\t Elevator -> %f\n",
                     u_outgoing.rudd, u_outgoing.flaps, u_outgoing.ail, u_outgoing.elev);
#endif
              poll_actuators->events = ZMQ_POLLOUT;
              poll_log->events = ZMQ_POLLOUT;
            }
          /* Clear the poll state. */
          poll_imu->revents = 0;
        }

      if (bail) die(bail);
      if (poll_actuators->revents & ZMQ_POLLOUT) {
          const void *bufs[] = {&u_outgoing};
          const uint32_t lens[] = {sizeof(u_outgoing)};
          const int zs = zmq_sendm(zsock_actuators, bufs, lens,
                                   sizeof(lens) / sizeof(lens[0]));
          if (zs < 0) {
              txfails++;
            } else {
#ifdef DEBUG
              printf("Sent to actuators!\n");
#endif
              /* Clear the events flag so we won't try to send until we
                   * have more data. */
              poll_actuators->events = 0;
              /* calculate next shot */

            }
          poll_actuators->revents = 0;
        }
      calc_next_shot(&t,rt_interval);


      /* I skipped logging; I think you know what to do. */
    }

  /* Shouldn't get here. */
  return 0;
}
