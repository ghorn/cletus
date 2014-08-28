/* Copyright 2014 Greg Horn <gregmainland@gmail.com>
 *
 * This file is hereby placed in the public domain, or, if your legal
 * system does not recognize such a concept, you may consider it
 * licensed under BSD 3.0.  Use it for good.
 */

#include "./misc.h"
#include <stdio.h>
#include <sys/mman.h>
#include <stdlib.h>



double floating_time(const timestamp_t * const t) {
    return (double)t->tsec + ((double)t->tnsec)/1e9;
}

double floating_ProtoTime(const Protobetty__Timestamp * const t) {
    return (double)t->tsec + ((double)t->tnsec)/1e9;
}

void gettime(timestamp_t * const t) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    t->tsec = ts.tv_sec;
    t->tnsec = ts.tv_nsec;
}

void get_protbetty_timestamp(Protobetty__Timestamp * const t) {
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

double calcCurrentLatencyProto(Protobetty__Timestamp * const ref) {
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
    struct timespec t_now;
    clock_gettime(CLOCK_MONOTONIC ,&t_now);
    if ((t->tv_sec < t_now.tv_sec) && (t->tv_nsec < t_now.tv_nsec))
        calc_next_shot(t,interval);


}

void * alloc_workbuf(int size)
{
    char *ptr;

    /* allocate some memory */
    ptr = malloc(size);

    /* return NULL on failure */
    if (ptr == NULL)
        return NULL;

    /* lock this buffer into RAM */
    if (mlock(ptr, size)) {
        free(ptr);
        return NULL;
    }
    return ptr;
}

void  free_workbuf(void *ptr, int size)
{
    /* unlock the address range */
    munlock(ptr, size);

    /* free the memory */
    free(ptr);
}




