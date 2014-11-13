// Copyright 2014, University of Freiburg
// Systemtheory Lab
// Author: Elias Rosch <eliasrosch@googlemail.com>

#include "./Sensor.h"

/*
 * Class for reading/setting Registervalues of the 
 * MPU9150 using I2C
 */

// Initializes the communication via i2c-1
void Sensor::initI2C(char devAddress) {
    char i2c_filename[40];
    sprintf(i2c_filename, "/dev/i2c-1");
    // Open devicefile
    if ((i2c_devfile = open(i2c_filename, O_RDWR)) < 0) {
        printf("Failed to open i2c device: %s\n", strerror(errno));
        exit(1);
    }

    // Set up ak8975-device
    if (ioctl(i2c_devfile, I2C_SLAVE, 0x0C) < 0) {
        printf("Failed to acquire bus: %s\n", strerror(errno));
        exit(1);
    }
    
    writeRegister(0x0A, 0x00); //PowerDownMode
    writeRegister(0x0A, 0x01); //Single Measurement Mode
    writeRegister(0x0A, 0x00); //PowerDownMode

    // Set up mpu9150-i2c-device address
    if (ioctl(i2c_devfile, I2C_SLAVE, devAddress) < 0) {
        printf("Failed to acquire bus: %s\n", strerror(errno));
        exit(1);
    }

    // Disable sleepmode of MPU9150
    writeRegister(PWR_MGMT_1,0x00);
    // Enable pass-through mode
    writeRegister(0x37, 0x02);
    // Disable master mode
    writeRegister(0x6A, 0x00);
}

void Sensor::writeRegister(char regAddress, char value) {
    char buf[2] = {regAddress, value};
    if (write(i2c_devfile, buf, 2) != 2) {
        printf("Error writing to i2c bus: %s\n", strerror(errno));
        printf("\n\n");
    }
}

char Sensor::readRegister(char regAddress) {
    char buf[1] = {regAddress};
    if (write(i2c_devfile, buf, 1) != 1) {
        printf("Error writing to i2c bus: %s\n", strerror(errno));
        printf("\n\n");
    }
    if (read(i2c_devfile, buf, 1) != 1) {
        printf("Error reading from i2c bus: %s\n", strerror(errno));
        printf("\n\n");
    }
    return buf[0];
}

int16_t Sensor::readValue(char highByte, char lowByte) {
    int16_t retValue = readRegister(highByte);
    retValue = retValue << 8;
    retValue = retValue + readRegister(lowByte);
    return retValue;
}

SensorValues* Sensor::getSensorValues(char datatype) {
    if (datatype == ACCEL_TYPE) {
        accel.compX = readValue(ACCEL_XOUT_H, ACCEL_XOUT_L);
        accel.compY = readValue(ACCEL_YOUT_H, ACCEL_YOUT_L);
        accel.compZ = readValue(ACCEL_ZOUT_H, ACCEL_ZOUT_L);
        return &accel;
    } else if (datatype == GYRO_TYPE) {
        gyro.compX = readValue(GYRO_XOUT_H, GYRO_XOUT_L);
        gyro.compY = readValue(GYRO_YOUT_H, GYRO_YOUT_L);
        gyro.compZ = readValue(GYRO_ZOUT_H, GYRO_ZOUT_L);
        return &gyro;
    } else if (datatype == MAG_TYPE) {    

        // Set up ak8975-device
        if (ioctl(i2c_devfile, I2C_SLAVE, 0x0C) < 0) {
            printf("Failed to acquire bus: %s\n", strerror(errno));
            exit(1);
        }
        writeRegister(0x0A,0x01);
        while(readRegister(0x02) == 0);
        mag.compX = readValue(MAG_XOUT_H, MAG_XOUT_L);
        mag.compY = readValue(MAG_YOUT_H, MAG_YOUT_L);
        mag.compZ = readValue(MAG_ZOUT_H, MAG_ZOUT_L);

        // Set up i2c-device
        if (ioctl(i2c_devfile, I2C_SLAVE, 0x68) < 0) {
            printf("Failed to acquire bus: %s\n", strerror(errno));
            exit(1);
        }

        return &mag;
    } else {
        printf("Error getting sensor values. Wrong datatype: %i\n", datatype);
        exit(1);
    }
}


