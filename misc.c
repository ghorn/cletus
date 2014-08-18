/* Copyright 2014 Greg Horn <gregmainland@gmail.com>
 *
 * This file is hereby placed in the public domain, or, if your legal
 * system does not recognize such a concept, you may consider it
 * licensed under BSD 3.0.  Use it for good.
 */

#include "./misc.h"
#include <stdio.h>
#include <sys/mman.h>



double floating_time(const timestamp_t * const t) {
  return (double)t->tsec + ((double)t->tnsec)/1e9;
}

double floating_ProtoTime(const TimestampProto * const t) {
  return (double)t->tsec + ((double)t->tnsec)/1e9;
}

void gettime(timestamp_t * const t) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  t->tsec = ts.tv_sec;
  t->tnsec = ts.tv_nsec;
}

double calcCurrentLatency(timestamp_t * const ref) {
  timestamp_t current;
  gettime(&current);
  return floating_time(&current) - floating_time(ref);
}

double calcCurrentLatencyProto(TimestampProto * const ref) {
  timestamp_t current;
  gettime(&current);
  return floating_time(&current) - floating_ProtoTime(ref);
}



//REALTIME METHODS
void stack_prefault(void) {

  unsigned char dummy[MAX_SAFE_STACK];

  memset(dummy, 0, MAX_SAFE_STACK);
  return;
}


/* Declare ourself as a real time task */
int set_priority(sched_param* const param, const int priority){
  param->sched_priority = priority;
  if(sched_setscheduler(0, SCHED_FIFO, param) == -1) {
      printf("sched_setscheduler failed\n");
      return(-1);
    }

  if(mlockall(MCL_CURRENT|MCL_FUTURE) == -1) {
      printf("mlockall failed \n");
      return(-2);
    }
  return 0;
}

void calc_next_shot(timespec* const t,const int interval)
{
  t->tv_nsec += interval;

  while (t->tv_nsec >= NSEC_PER_SEC) {
      t->tv_nsec -= NSEC_PER_SEC;
      t->tv_sec++;
    }
}



