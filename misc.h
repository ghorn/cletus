/* Copyright 2014 Greg Horn <gregmainland@gmail.com>
 *
 * This file is hereby placed in the public domain, or, if your legal
 * system does not recognize such a concept, you may consider it
 * licensed under BSD 3.0.  Use it for good.
 */
#ifndef __MISC_H__
#define __MISC_H__

#include <time.h>
#include "./structures.h"
#include <string.h>
#include <sched.h>

typedef struct sched_param  sched_param;
typedef struct timespec timespec;


#define MAX_SAFE_STACK (8*1024) /* The maximum stack size which is
                                   guaranteed safe to access without
                                   faulting */

#define NSEC_PER_SEC    (1000000000) /* The number of nsecs per sec. */


double floating_time(const timestamp_t * const t);
void gettime(timestamp_t * const t);
void stack_prefault(void);
int set_priority(sched_param* const param, const int priority);
void calc_next_shot(timespec* const t, const int interval);





#endif // __MISC_H__
