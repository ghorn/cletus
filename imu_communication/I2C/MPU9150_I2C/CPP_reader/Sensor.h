// Copyright 2014, University of Freiburg
// Systemtheory Lab
// Author: Elias Rosch <eliasrosch@googlemail.com>

#ifndef CPP_READER_SENSOR_H
#define CPP_READER_SENSOR_H

//#include<glib.h>
//#include<glib/gprintf.h>
#include<errno.h>
#include<string.h>
#include<stdio.h>
#include<stdlib.h>
#include<unistd.h>
#include<stdint.h>
#include<linux/i2c-dev.h>
#include<sys/ioctl.h>
#include<sys/types.h>
#include<sys/stat.h>
#include<fcntl.h>


// Define register addresses of MPU9150
#define PWR_MGMT_1 0x6B

#define ACCEL_XOUT_H 0x3B
#define ACCEL_XOUT_L 0x3C
#define ACCEL_YOUT_H 0x3D
#define ACCEL_YOUT_L 0x3E
#define ACCEL_ZOUT_H 0x3F
#define ACCEL_ZOUT_L 0x40

#define GYRO_XOUT_H 0x43
#define GYRO_XOUT_L 0x44
#define GYRO_YOUT_H 0x45
#define GYRO_YOUT_L 0x46
#define GYRO_ZOUT_H 0x47
#define GYRO_ZOUT_L 0x48

#define MAG_XOUT_H 0x04
#define MAG_XOUT_L 0x03
#define MAG_YOUT_H 0x06
#define MAG_YOUT_L 0x05
#define MAG_ZOUT_H 0x08
#define MAG_ZOUT_L 0x07

// Define datatypes
#define ACCEL_TYPE 0x00
#define GYRO_TYPE 0x01
#define MAG_TYPE 0x02


/*
 * This struct stores the recent values of the three
 * spacial components of different Sensors
 * (accelerometer, gyrometer, magnetometer) 
 */
struct SensorValues {
    int16_t compX;
    int16_t compY;
    int16_t compZ;
};

/*
 * Class for reading/setting Registervalues of the 
 * MPU9150 using I2C
 */
class Sensor {
    public:
        // Functions to communicate with the Sensors
        void initI2C(char devAddress);
        char readRegister(char regAddress);
        void writeRegister(char regAddress, char value);
        int16_t readValue(char highByte, char lowByte);
        SensorValues* getSensorValues(char datatype);
        // This buffer is for error handling
        char err_buffer;
        // Stores our devicefile information
        int i2c_devfile;
        // Variables storing the recent values of the Sensors
        struct SensorValues accel;
        struct SensorValues gyro;
        struct SensorValues mag;
};

#endif // CPP_READER_SENSOR_H
