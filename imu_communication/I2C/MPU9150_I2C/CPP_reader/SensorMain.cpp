// Copyright 2014, University of Freiburg
// Systemtheory Lab
// Author: Elias Rosch <eliasrosch@googlemail.com>

#include "./Sensor.h"

int main (int argc, char** argv) {
    Sensor mpu9150;
    mpu9150.initI2C(0x68);
    for(int i = 0; ; i++) {
        SensorValues* sensorvalues;
        sensorvalues = mpu9150.getSensorValues(ACCEL_TYPE);
        printf("\nAccelerometer Values:\n");
        printf("x-component: %i\n", sensorvalues->compX);
        printf("y-component: %i\n", sensorvalues->compY);
        printf("z-component: %i\n", sensorvalues->compZ);
        sensorvalues = mpu9150.getSensorValues(MAG_TYPE);
        printf("\nMagnetometer Values:\n");
        printf("x-component: %i\n", sensorvalues->compX);
        printf("y-component: %i\n", sensorvalues->compY);
        printf("z-component: %i\n", sensorvalues->compZ);
        //printf("%i\n",mpu9150.readValue(ACCEL_XOUT_H,ACCEL_XOUT_L));
    }
    return 0;
}
