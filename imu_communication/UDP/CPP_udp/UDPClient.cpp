// Copyright 2014, University of Freiburg
// Systems Theory Lab
// Author: Elias Rosch <eliasrosch@googlemail.com>

#include <sys/time.h>
#include <iostream>
#include "./UDP.h"

int main (int argc, char** argv) {
    // Define number of samples
    int numOfSamples = 2000;
    // Declare and initialize the Sensordevice
    UDP my_udp;
    my_udp.initUDP();
    // Variables for time measurement
    timeval startTime, endTime;
    // Start time measure
    gettimeofday(&startTime, 0);
    
    for(int16_t i = 0;i < numOfSamples ; i++) {
        // Do one send operation
        my_udp.sendUDP(i);
        //int16_t value = my_udp.receiveUDP();
        //printf("The received value is: %i\n", value);
        // Measure the interval between 2 samples
        //gettimeofday(&endTime, 0);
        //float neededSeconds = (endTime.tv_sec - startTime.tv_sec) + 0.000001 * (endTime.tv_usec - startTime.tv_usec);

    }
    // Stop time measurement
    gettimeofday(&endTime, 0);
    float neededSeconds = (endTime.tv_sec - startTime.tv_sec)
                  + 0.000001 * (endTime.tv_usec - startTime.tv_usec);
    std::cout << "Number of samples: " << numOfSamples << "\tTime needed: " << neededSeconds << " s" << std::endl;
    std::cout << "Average per sample: " << 1000*neededSeconds/numOfSamples << " ms" << std::endl;
    return 0;
}
