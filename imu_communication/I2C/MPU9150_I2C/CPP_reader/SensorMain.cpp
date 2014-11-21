// Copyright 2014, University of Freiburg
// Systemtheory Lab
// Author: Elias Rosch <eliasrosch@googlemail.com>

#include <sys/time.h>
#include <iostream>
#include "./Sensor.h"

int main (int argc, char** argv) {
    // Define number of samples
    int numOfSamples = 2000;
    // Declare and initialize the Sensordevice
    Sensor mpu9150;
    mpu9150.initI2C(ACCEL_GYRO_DEVICE, MAG_DEVICE);
    // Declare a structure that contains measurements of each spacial component
    SensorValues* sensorvalues;
    // Variables for time measurement
    timeval startTime, endTime;
    // Start time measure
    gettimeofday(&startTime, 0);
    for(int i = 0;i < numOfSamples ; i++) {
        // Do one measurement
        sensorvalues = mpu9150.getSensorValues(ACCEL_TYPE);
        // Measure the interval between 2 samples
        gettimeofday(&endTime, 0);
        float neededSeconds = (endTime.tv_sec - startTime.tv_sec) + 0.000001 * (endTime.tv_usec - startTime.tv_usec);
        printf("%f;%i;%i;%i\n", neededSeconds, sensorvalues->compX, sensorvalues->compY, sensorvalues->compZ);

        /*
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
        */
    }
    // Stop time measurement
    gettimeofday(&endTime, 0);
    float neededSeconds = (endTime.tv_sec - startTime.tv_sec)
                  + 0.000001 * (endTime.tv_usec - startTime.tv_usec);
    std::cout << "Number of samples: " << numOfSamples << "\tTime needed: " << neededSeconds << " s" << std::endl;
    std::cout << "Average per sample: " << 1000*neededSeconds/numOfSamples << " ms" << std::endl;
    return 0;
}
