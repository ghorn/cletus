/* Copyright 2014 Matt Peddie <peddie@alum.mit.edu>
 *
 * This file is hereby placed in the public domain, or, if your legal
 * system does not recognize such a concept, you may consider it
 * licensed under BSD 3.0.  Use it for good.
 */
#ifndef __STRUCTURES_H__
#define __STRUCTURES_H__

#include <inttypes.h>

typedef struct {
  uint64_t tsec;
  uint64_t tnsec;
} timestamp_t;

typedef struct {
  double x;
  double y;
  double z;
} xyz_t;

typedef struct {
  timestamp_t timestamp;
  xyz_t gyro;
  xyz_t accel;
  xyz_t gps_pos;
  xyz_t gps_vel;
} sensors_t;

typedef struct {
  timestamp_t start;
  timestamp_t stop;
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
