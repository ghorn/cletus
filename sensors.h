/* Copyright 2014 Greg Horn <gregmainland@gmail.com>
 *
 * This file is hereby placed in the public domain, or, if your legal
 * system does not recognize such a concept, you may consider it
 * licensed under BSD 3.0.  Use it for good.
 */

#ifndef __SENSORS_H__
#define __SENSORS_H__
#include "./structures.h"
#include "./protos_c/messages.pb-c.h"
#include "./piksi/piksi.h"


#define MAX_STREAM_SIZE 255
#define MAX_OUTPUT_STREAM_SIZE 36
#define INPUT_BUFFER_SIZE 255


void raw_to_protobuf(const xyz_int *const source, Protobetty__Xyz *dest);
void scaled_to_protobuf(const xyz_int *const source, Protobetty__Xyz *dest, double coef);
void copy_timestamp(const timestamp_t *const source, Protobetty__Timestamp *dest);
char * toArray(int number);




#endif // __SENSORS_H__
