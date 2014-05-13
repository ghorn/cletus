/* Copyright 2014 Matt Peddie <peddie@alum.mit.edu>
 *
 * This file is hereby placed in the public domain, or, if your legal
 * system does not recognize such a concept, you may consider it
 * licensed under BSD 3.0.  Use it for good.
 */
#ifndef __STRUCTURES_H__
#define __STRUCTURES_H__

#include <inttypes.h>
#include <time.h>

typedef struct {
  double x;
  double y;
  double z;
} xyz_t;

typedef struct {
  struct timespec timestamp;
  xyz_t gyro;
  xyz_t accel;
  xyz_t gps_pos;
  xyz_t gps_vel;
} sensors_t;

typedef struct {
  struct timespec start;
  struct timespec stop;
  double flaps;
  double ail;
  double rudd;
  double elev;
} actuators_t;

#define LOG_MESSAGE_SENSORS 0
#define LOG_MESSAGE_ESTIMATOR 1
#define LOG_MESSAGE_CONTROLLER 2
#define LOG_MESSAGE_ACTUATORS 3

#endif  /* __STRUCTURES_H__ */
