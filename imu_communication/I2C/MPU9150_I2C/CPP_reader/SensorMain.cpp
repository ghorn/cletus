// Copyright 2014, University of Freiburg
// Systemtheory Lab
// Author: Elias Rosch <eliasrosch@googlemail.com>

#include "./Sensor.h"

int main (int argc, char** argv) {
    Sensor mpu9150;
    mpu9150.initI2C(ACCEL_GYRO_DEVICE, MAG_DEVICE);
    for(int i = 0; ; i++) {
        SensorValues* sensorvalues;
        // Read data from Accelerometer
        sensorvalues = mpu9150.getSensorValues(ACCEL_TYPE);
        printf("\nAccelerometer Values:\n");
        printf("x-component: %i\n", sensorvalues->compX);
        printf("y-component: %i\n", sensorvalues->compY);
        printf("z-component: %i\n", sensorvalues->compZ);
        // Read data from Gyrometer
        sensorvalues = mpu9150.getSensorValues(GYRO_TYPE);
        printf("\nGyrometer Values:\n");
        printf("x-component: %i\n", sensorvalues->compX);
        printf("y-component: %i\n", sensorvalues->compY);
        printf("z-component: %i\n", sensorvalues->compZ);
        // Read data from Magnetometer
        sensorvalues = mpu9150.getSensorValues(MAG_TYPE);
        printf("\nMagnetometer Values:\n");
        printf("x-component: %i\n", sensorvalues->compX);
        printf("y-component: %i\n", sensorvalues->compY);
        printf("z-component: %i\n", sensorvalues->compZ);
    }
    return 0;
}
