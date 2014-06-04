/* Copyright 2014 Greg Horn <gregmainland@gmail.com>
 *
 * This file is hereby placed in the public domain, or, if your legal
 * system does not recognize such a concept, you may consider it
 * licensed under BSD 3.0.  Use it for good.
 */

#ifndef __SENSORS_H__
#define __SENSORS_H__
#include "./structures.h"

#include "./lisa_communication/uart_communication.h"
#include "./lisa_communication/data_decoding.h"


#define CBSIZE 1024 * 16
#define OUTPUT_BUFFER 36
#define MAX_STREAM_SIZE 255
#define MAX_OUTPUT_STREAM_SIZE 36
#define INPUT_BUFFER_SIZE 255


void get_sensors(sensors_t * const y);
int get_lisa_data(sensors_t * const data, uint8_t input_buffer[]);


#endif // __SENSORS_H__
