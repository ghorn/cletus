/* Copyright 2014 Matt Peddie <peddie@alum.mit.edu>
 *
 * This file is hereby placed in the public domain, or, if your legal
 * system does not recognize such a concept, you may consider it
 * licensed under BSD 3.0.  Use it for good.
 */
#ifndef __STRUCTURES_H__
#define __STRUCTURES_H__

#include <inttypes.h>

//DATATYPES
typedef struct {
  uint64_t tsec;
  uint64_t tnsec;
} timestamp_t;

typedef struct {
  double x;
  double y;
  double z;
} xyz_double;

typedef struct {
  int x;
  int y;
  int z;
} xyz_int;

typedef struct{
  int qi;
  int qx;
  int qy;
  int qz;
}quaternion_t;

typedef struct{
  double qi;
  double qx;
  double qy;
  double qz;
}quaternion__double_t;


//SENSORS STRUCTS
typedef struct
{
  xyz_int data;
  timestamp_t timestamp;
}gyro_raw_t;

typedef struct
{
  xyz_int data;
  timestamp_t timestamp;
}mag_raw_t;

typedef struct
{
  xyz_int data;
  timestamp_t timestamp;
}accel_raw_t;

typedef struct
{
  xyz_double data;
  timestamp_t timestamp;
}gyro_scaled_t;

typedef struct
{
  xyz_double data;
  timestamp_t timestamp;
}mag_scaled_t;

typedef struct
{
  xyz_double data;
  timestamp_t timestamp;
}accel_scaled_t;

typedef struct{
  xyz_int pos_data;
  xyz_int vel_data;
  timestamp_t timestamp;
}gps_t;

typedef struct{
  accel_raw_t imu_accel;
  accel_scaled_t imu_accel_scaled;
  gyro_raw_t imu_gyro;
  gyro_scaled_t imu_gyro_scaled;
  mag_raw_t imu_mag;
  mag_scaled_t imu_mag_scaled;
}imu_t;

typedef struct{
  quaternion_t imu;
  quaternion_t body;
  timestamp_t timestamp;
  quaternion__double_t imu_converted;
  quaternion__double_t body_converted;
}ahrs_t;



typedef struct {
  imu_t imu;
  gps_t gps;
  ahrs_t ahrs;
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
