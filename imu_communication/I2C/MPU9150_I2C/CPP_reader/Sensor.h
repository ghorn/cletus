// Copyright 2014, University of Freiburg
// Systems Theory Lab
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


// Define i2c-device-addresses of MPU9150
#define ACCEL_GYRO_DEVICE 0x68
#define MAG_DEVICE 0x0C

// Define register addresses of MPU9150 (Magnetometer registers are not included)
#define PWR_MGMT_1 0x6B
#define INT_PIN_CFG 0x37
#define USER_CTRL 0x6A


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


//Define Registers of Magnetometer device
#define MAG_CNTL 0x0A
#define MAG_ST1 0x02
#define MAG_XOUT_H 0x04
#define MAG_XOUT_L 0x03
#define MAG_YOUT_H 0x06
#define MAG_YOUT_L 0x05
#define MAG_ZOUT_H 0x08
#define MAG_ZOUT_L 0x07

// Define datatypes as the starting address for that type of data
#define ACCEL_TYPE ACCEL_XOUT_H
#define GYRO_TYPE GYRO_XOUT_H
#define MAG_TYPE MAG_XOUT_L


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
        // Constructor/Destructor
        Sensor();
        ~Sensor();
        // Functions to communicate with the Sensors
        void initI2C(char accel_gyro_address, char mag_address);
        
        // Reads the value of the register with given address
        char readRegister(char regAddress);
        
        // Writes a value to the register with given address
        void writeRegister(char regAddress, char value);
        
        // Reads 2 bytes starting at given address
        int16_t readComp(char regAddress);

        /* Reads 6 Bytes starting at given address
         * So information of all spacial components 
         * of one type of sensor are fetched at the same time.
         * Parameter is either the type of sensor 
         * or starting address of specific sensor register
         */
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
