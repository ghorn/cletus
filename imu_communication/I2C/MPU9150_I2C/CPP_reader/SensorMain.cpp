// Copyright 2014, University of Freiburg
// Systemstheory Lab
// Author: Elias Rosch <eliasrosch@googlemail.com>

#include "./Sensor.h"

int main (int argc, char** argv) {
    Sensor mpu9150;
    mpu9150.initI2C(0x68);
    for(int i = 0; ; i++) {
        printf("%i\n",mpu9150.readValue(ACCEL_XOUT_H,ACCEL_XOUT_L));
    }
    return 0;
}
