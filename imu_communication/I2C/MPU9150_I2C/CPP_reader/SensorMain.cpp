// Copyright 2014, University of Freiburg
// Systemtheory Lab
// Author: Elias Rosch <eliasrosch@googlemail.com>

#include <sys/time.h>
#include <iostream>
#include "./Sensor.h"

int main (int argc, char** argv) {
    int numOfSamples = 20000;
    Sensor mpu9150;
    mpu9150.initI2C(ACCEL_GYRO_DEVICE, MAG_DEVICE);
    SensorValues* sensorvalues;
    timeval startTime, endTime;
    gettimeofday(&startTime, 0);
    for(int i = 0;i < numOfSamples ; i++) {
        // Read data from Accelerometer
        sensorvalues = mpu9150.getSensorValues(GYRO_TYPE);
        gettimeofday(&endTime, 0);
        float neededSeconds = (endTime.tv_sec - startTime.tv_sec) + 0.000001 * (endTime.tv_usec - startTime.tv_usec);
        printf("%f;%i;%i;%i\n", neededSeconds, sensorvalues->compX, sensorvalues->compY, sensorvalues->compZ);
        /*
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
        */
    }
    /*
    gettimeofday(&endTime, 0);
    float neededSeconds = (endTime.tv_sec - startTime.tv_sec)
                  + 0.000001 * (endTime.tv_usec - startTime.tv_usec);
    std::cout << "Number of samples: " << numOfSamples << "\tTime needed: " << neededSeconds << " s" << std::endl;
    std::cout << "Average per sample: " << 1000*neededSeconds/numOfSamples << " ms" << std::endl;
    */
    return 0;
}
