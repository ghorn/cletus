/* Copyright 2014 Greg Horn <gregmainland@gmail.com>
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
#include <time.h>
#include <math.h>

#include "./zmq.h"
#include "./comms.h"
#include "./structures.h"
#include "./log.h"
#include "./sensors.h"
#include "./misc.h"

void some_data(xyz_t * v, double t, double scalar);
void some_data(xyz_t * v, double t, double scalar){
  v->x = sin(t*scalar);
  v->y = sin(t*scalar*2);
  v->z = sin(t*scalar*3);
}

void get_sensors(sensors_t * const y) {
  clock_gettime(CLOCK_MONOTONIC, &(y->timestamp));
  double t = floating_time(&(y->timestamp));
  some_data(&(y->gyro), t, 2);
  some_data(&(y->accel), t, 3);
  some_data(&(y->gps_pos), t, 4);
  some_data(&(y->gps_vel), t, 5);
  printf("read sensors!: %.4f\n",t);
}
