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



  //Setting up priority
  struct sched_param param;
  set_priority(&param, 48);
  stack_prefault();
  //Timer variables
  struct timespec start, finish;


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
  //We need just one socket for getting lisa messages
  zsock_data = setup_zmq_receiver(LISA_CHAN, &zctx, ZMQ_SUB, NULL, 1, 500);
  if (NULL == zsock_data)
    die(1);

  //also only one pollitem polling from LISA_CHANNEL
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

  double elapsed;

  //Check how exact timing will be for sleep times of 1 second
  for (int i =0 ; i < 10; i++)
    {
      clock_gettime(CLOCK_MONOTONIC, &start);
      usleep(1000000);
      clock_gettime(CLOCK_MONOTONIC, &finish);
      elapsed = (finish.tv_sec - start.tv_sec);
      elapsed += (finish.tv_nsec - start.tv_nsec) / 1000000000.0;
      printf("Measured Time for sleeping 1 second =  %f \n",elapsed);
    }

  double sum_elapsed = 0.0;
  elapsed =0;
  unsigned long counter = 0;
  unsigned char msg_ID;


  //***********************************************************
  // MAIN LOOP
  //*********************************************************
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
          zmq_recv(zsock_data,&msg_ID,1,0);
          if (msg_ID ==  131)
            {
              clock_gettime(CLOCK_MONOTONIC, &finish);
              elapsed = (finish.tv_nsec - start.tv_nsec) / 1000000000.0;
              sum_elapsed += elapsed;

              clock_gettime(CLOCK_MONOTONIC, &start);
              poll_data->revents = 0;
              counter++;
            }
        }

      printf("\r\n\bReceived Messages: %lu [ID:%u] \tPeriod: %f s \t[AVG: %f]\tFrequency %f\t[AVG: %f]",
             counter, msg_ID, elapsed, sum_elapsed/counter, 1.0/elapsed, 1.0/(sum_elapsed/counter));

      /* I skipped logging; I think you know what to do. */
    }

  /* Shouldn't get here. */
  return 0;
}

