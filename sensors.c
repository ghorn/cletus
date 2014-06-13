/* Copyright 2014 Greg Horn <gregmainland@gmail.com>
 *
 * This file is hereby placed in the public domain, or, if your legal
 * system does not recognize such a concept, you may consider it
 * licensed under BSD 3.0.  Use it for good.
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <signal.h>
#include <zmq.h>
#include <math.h>

#include "./zmq.h"
#include "./comms.h"
#include "./structures.h"
#include "./log.h"
#include "./sensors.h"
#include "./misc.h"








void some_data(xyz_double * v, double t, double scalar);
void some_data(xyz_double * v, double t, double scalar){
  v->x = sin(t*scalar);
  v->y = sin(t*scalar*2);
  v->z = sin(t*scalar*3);
}



//void get_sensors(sensors_t * const y) {
////  gettime(&(y->timestamp));
////  double t = floating_time(&(y->timestamp));
////  some_data(&(y->gyro.data), t, 2);
////  some_data(&(y->accel.data), t, 3);
////  printf("read sensors!: %.4f\n",t);
//}


int get_lisa_data(sensors_t * const data, uint8_t input_buffer[]) {
  int message_length = serial_input_get_lisa_data(input_buffer); //blocking !!!
  if(message_length > 0){
      //add timestamp
      message_length=add_timestamp(input_buffer);
      //Decode messages to see what we receive
      int err_decode = data_decode(input_buffer);
      if (err_decode != DEC_ERR_NONE)
        {
          printf("Error while decoding data messages.");
          return 0;
        }
      get_new_sensor_struct(data);
      return input_buffer[MESSAGE_ID_INDEX];
    }
  return 0;
}

void xyz_convert_to_double(const xyz_int *const source, xyz_double* dest, double coef)
{
  dest->x = (double)source->x / coef;
  dest->y = (double)source->y / coef;
  dest->z = (double)source->z / coef;
}

void quat_convert_to_double(const quaternion_t *const source, quaternion__double_t* dest, double coef)
{
  dest->qi = (double)source->qi / coef;
  dest->qx = (double)source->qx / coef;
  dest->qy = (double)source->qy / coef;
  dest->qz = (double)source->qz / coef;
}

