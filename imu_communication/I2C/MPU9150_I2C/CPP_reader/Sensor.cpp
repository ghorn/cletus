// Copyright 2014, University of Freiburg
// Systemstheory Lab
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
    // Set up i2c-device address
    if (ioctl(i2c_devfile, I2C_SLAVE, devAddress) < 0) {
        printf("Failed to acquire bus: %s\n", strerror(errno));
        exit(1);
    }
    // Disable sleepmode of MPU9150
    writeRegister(PWR_MGMT_1,0x00);
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


