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

typedef struct{
  short roll;
  short pitch;
  short yaw;
  short mode;
  short kill;
  char status;
} rc_t;

typedef struct {
  imu_t imu;
  gps_t gps;
  ahrs_t ahrs;
  rc_t rc;
} sensors_t;

typedef struct {
  timestamp_t start;
  timestamp_t stop;
  double flaps;
  double ail;
  double rudd;
  double elev;
} actuators_t;

typedef struct {
  int16_t servo_1;
  int16_t servo_2;
  int16_t servo_3;
  int16_t servo_4;
  int16_t servo_5;
  int16_t servo_6;
  int16_t servo_7;
} servo_message_t;

typedef struct{
  uint8_t startbyte;
  uint8_t length;
  uint8_t sender_id;
  uint8_t message_id;
  servo_message_t servos_msg;
  uint8_t checksum1;
  uint8_t checksum2;
  }lisa_message_t;

#define LOG_MESSAGE_SENSORS 0
#define LOG_MESSAGE_ESTIMATOR 1
#define LOG_MESSAGE_CONTROLLER 2
#define LOG_MESSAGE_ACTUATORS 3

#endif  /* __STRUCTURES_H__ */
