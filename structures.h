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
typedef struct __attribute__((packed)){
    uint64_t tsec;
    uint64_t tnsec;
} timestamp_t;

typedef struct __attribute__((packed)){
    double x;
    double y;
    double z;
} xyz_double;

typedef struct __attribute__((packed)){
    int32_t x;
    int32_t y;
    int32_t z;
} xyz_int;

typedef struct __attribute__((packed)){
    int32_t qi;
    int32_t qx;
    int32_t qy;
    int32_t qz;
}quaternion_t;

typedef struct __attribute__((packed)){
    double qi;
    double qx;
    double qy;
    double qz;
}quaternion_double_t;


typedef struct __attribute__((packed)){
    uint8_t message_length;
    uint8_t sender_id;
    uint8_t message_id;
} message_header_t;

typedef struct __attribute__((packed)){
    uint8_t checksum1;
    uint8_t checksum2;
} message_footer_t;

//SENSORS STRUCTS

typedef struct __attribute__((packed))
{
    uint8_t length;
    uint8_t sender_id;
    uint8_t msg_id;
}lisa_header_t;

typedef struct __attribute__((packed))
{
    uint8_t id;
    xyz_int pos_data;
    xyz_int vel_data;
    timestamp_t timestamp;
}gps_t;

typedef struct __attribute__((packed))
{
    lisa_header_t header;
    uint16_t adc;
    uint16_t offset;
    float scaled;
}airspeed_t;



typedef struct __attribute__((packed))
{
    lisa_header_t header;
    xyz_int data;
}imu_raw_t;

typedef struct __attribute__((packed)){
    quaternion_t imu;
    quaternion_t body;
    timestamp_t timestamp;
}ahrs_int_t;

typedef struct __attribute__((packed)){
    quaternion_double_t imu_converted;
    quaternion_double_t body_converted;
    timestamp_t timestamp;
}ahrs_double_t;




typedef struct __attribute__((packed)){
    int8_t roll;
    int8_t pitch;
    int8_t yaw;
    int8_t mode;
    int8_t kill;
    uint8_t status;
} rc_t;

typedef struct __attribute__((packed)) {
    imu_raw_t imu_raw;
    gps_t gps;
    ahrs_int_t ahrs_int;
    ahrs_double_t ahrs_double;
    rc_t rc;
    airspeed_t airspeed_raw;
} lisa_messages_t;

typedef struct __attribute__((packed)) {
    timestamp_t start;
    timestamp_t stop;
    double flaps;
    double ail;
    double rudd;
    double elev;
} actuators_t;

typedef struct __attribute__((packed)) {
    int16_t servo_1;
    int16_t servo_2;
    int16_t servo_3;
    int16_t servo_4;
    int16_t servo_5;
    int16_t servo_6;
    int16_t servo_7;
} servo_message_t;

typedef struct __attribute__((packed)){
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
