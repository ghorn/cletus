// Copyright 2014, University of Freiburg
// Systemtheory Lab
// Author: Elias Rosch <eliasrosch@googlemail.com>

#include "./Sensor.h"

/*
 * Class for reading/setting Registervalues of the 
 * MPU9150 using I2C
 */

// Initializes the communication via i2c-1
void Sensor::initI2C(char accel_gyro_address, char mag_address) {
    char i2c_filename[40];
    sprintf(i2c_filename, "/dev/i2c-1");
    // Open devicefile
    if ((i2c_devfile = open(i2c_filename, O_RDWR)) < 0) {
        printf("Failed to open i2c device: %s\n", strerror(errno));
        exit(1);
    }

    // Set up ak8975-device
    if (ioctl(i2c_devfile, I2C_SLAVE, mag_address) < 0) {
        printf("Failed to acquire bus: %s\n", strerror(errno));
        exit(1);
    }
    
    writeRegister(0x0A, 0x00); //PowerDownMode
    writeRegister(0x0A, 0x01); //Single Measurement Mode
    writeRegister(0x0A, 0x00); //PowerDownMode

    // Set up mpu9150-i2c-device address
    if (ioctl(i2c_devfile, I2C_SLAVE, accel_gyro_address) < 0) {
        printf("Failed to acquire bus: %s\n", strerror(errno));
        exit(1);
    }

    // Disable sleepmode of MPU9150
    writeRegister(PWR_MGMT_1, 0x00);
    // Enable pass-through mode
    writeRegister(INT_PIN_CFG, 0x02);
    // Disable master mode
    writeRegister(USER_CTRL, 0x00);
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

int16_t Sensor::readComp(char regAddress) {
    char buf[2] = {regAddress};
    if (write(i2c_devfile, buf, 1) != 1) {
        printf("Error writing to i2c bus: %s\n", strerror(errno));
        printf("\n\n");
    }
    if (read(i2c_devfile, buf, 2) != 2) {
        printf("Error reading from i2c bus: %s\n", strerror(errno));
        printf("\n\n");
    }
    int16_t retValue = buf[0];
    retValue = retValue << 8;
    retValue = retValue + buf[1];
    return retValue;
}

SensorValues* Sensor::getSensorValues(char datatype) {
    if (datatype == ACCEL_TYPE) {
        accel.compX = readComp(ACCEL_XOUT_H);
        accel.compY = readComp(ACCEL_YOUT_H);
        accel.compZ = readComp(ACCEL_ZOUT_H);
        return &accel;
    } else if (datatype == GYRO_TYPE) {
        gyro.compX = readComp(GYRO_XOUT_H);
        gyro.compY = readComp(GYRO_YOUT_H);
        gyro.compZ = readComp(GYRO_ZOUT_H);
        return &gyro;
    } else if (datatype == MAG_TYPE) {    
        // Change i2c-device-address to magnetometer
        if (ioctl(i2c_devfile, I2C_SLAVE, MAG_DEVICE) < 0) {
            printf("Failed to acquire bus: %s\n", strerror(errno));
            exit(1);
        }
        // Send measurement request to the magnetometer
        writeRegister(MAG_CNTL,0x01);
        // Wait for data_ready-flag
        while(readRegister(MAG_ST1) == 0);
        // read values of registers
        int16_t helper = readComp(MAG_XOUT_L);
        mag.compX = (helper & 0xFF00) >> 8;
        mag.compX = mag.compX + ((helper & 0x00FF) << 8);
        
        helper = readComp(MAG_YOUT_L);
        mag.compY = (helper & 0xFF00) >> 8;
        mag.compY = mag.compY + ((helper & 0x00FF) << 8);

        helper = readComp(MAG_ZOUT_L);
        mag.compZ = (helper & 0xFF00) >> 8;
        mag.compZ = mag.compZ + ((helper & 0x00FF) << 8);

        // Change i2c-device-address back to accel_gyro
        if (ioctl(i2c_devfile, I2C_SLAVE, ACCEL_GYRO_DEVICE) < 0) {
            printf("Failed to acquire bus: %s\n", strerror(errno));
            exit(1);
        }
        return &mag;
    } else {
        printf("Error getting sensor values. Wrong datatype: %i\n", datatype);
        exit(1);
    }
}
