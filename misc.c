/* Copyright 2014 Greg Horn <gregmainland@gmail.com>
 *
 * This file is hereby placed in the public domain, or, if your legal
 * system does not recognize such a concept, you may consider it
 * licensed under BSD 3.0.  Use it for good.
 */
#include <time.h>

#include "./misc.h"

double floating_time(const struct timespec * const t) {
  return (double)t->tv_sec + ((double)t->tv_nsec)/1e9;
}
