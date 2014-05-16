/* Copyright 2014 Greg Horn <gregmainland@gmail.com>
 *
 * This file is hereby placed in the public domain, or, if your legal
 * system does not recognize such a concept, you may consider it
 * licensed under BSD 3.0.  Use it for good.
 */
#include <time.h>

#include "./misc.h"
#include "./structures.h"

double floating_time(const timestamp_t * const t) {
  return (double)t->tsec + ((double)t->tnsec)/1e9;
}

void gettime(timestamp_t * const t) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  t->tsec = ts.tv_sec;
  t->tnsec = ts.tv_nsec;
}
