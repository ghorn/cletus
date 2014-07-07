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





/* ZMQ resources */
static void *zctx = NULL;
static void *zsock_data = NULL;


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


//  struct sched_param param;
//  set_priority(&param, 49);
//  stack_prefault();
  struct timespec t, start, finish;


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

  zsock_data = setup_zmq_receiver(LISA_CHAN, &zctx, ZMQ_SUB, NULL, 1, 500);
  if (NULL == zsock_data)
    die(1);


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

  clock_gettime(CLOCK_MONOTONIC ,&t);
  /* start after one second */
  t.tv_sec++;



  //Measured value might also be not super accurate but at least we can see if we are in the right region
  double elapsed;
  for (int i =0 ; i < 10; i++)
    {
      clock_gettime(CLOCK_MONOTONIC, &start);
      usleep(1000000);
      clock_gettime(CLOCK_MONOTONIC, &finish);
      elapsed = (finish.tv_sec - start.tv_sec);
      elapsed += (finish.tv_nsec - start.tv_nsec) / 1000000000.0;
      printf("Measured Time for sleeping 1 second =  %f \n",elapsed);
    }

  double sum_elapsed =0;
  long unsigned int counter = 0;


  /* Here's the main loop -- we only do stuff when input or output
       * happens.  The timeout can be put to good use, or you can also use
       * timerfd_create() to create a file descriptor with a timer under
       * the hood and dispatch on that.
       *
       * I apologize for the length of this loop.  For production code,
       * you'd want to pull most of the actual handling out into functions
       * and simply loop over your polls; I've left it all inline here
       * mostly out of laziness. */
printf("Starting Test: \n\n");
clock_gettime(CLOCK_MONOTONIC, &start);
  for (;;) {
      if (bail) die(bail);
      /* Poll for activity; time out after 10 milliseconds. */


      const int polled = zmq_poll(polls, npolls, 0);
      if (polled < 0) {
          if (bail) die(bail);
          zerr("while polling");
          continue;
        } else if (polled == 0) {
          if (bail) die(bail);
          continue;
        }

      if (bail) die(bail);
      if (poll_data->revents & ZMQ_POLLIN)
        {
          clock_gettime(CLOCK_MONOTONIC, &finish);
          elapsed = (finish.tv_nsec - start.tv_nsec) / 1000000000.0;
          sum_elapsed += elapsed;
          clock_gettime(CLOCK_MONOTONIC, &start);
          poll_data->revents = 0;
          counter++;
        }

      printf("\r\n\bReceived Messages: %lu \tPeriod: %f s \t[AVG: %f]\tFrequency %f\t[AVG: %f]",
             counter, elapsed, sum_elapsed/counter, 1.0/elapsed, 1.0/(sum_elapsed/counter));




      /* I skipped logging; I think you know what to do. */
    }

  /* Shouldn't get here. */
  return 0;
}

