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
#include "./controller.h"
#include "./misc.h"


//void run_controller(const sensors_t * const y, actuators_t * const u) {
////  gettime(&(u->start));
////  static double integral_term = 0;
////  static double reference = 4;
////  static int counter = 0;
////  counter++;
////  if (counter == 10) {
////      reference = -reference;
////      counter = 0;
////    }

////  integral_term += reference*0.1 + y->imu.imu_gyro_scaled.data.x;
////  u->flaps = sin(1*integral_term);
////  u->ail   = sin(2*integral_term);
////  u->rudd  = sin(3*integral_term);
////  u->elev  = sin(4*integral_term);
////  gettime(&(u->stop));
////  double t0 = floating_time(&(u->start));
////  double tf = floating_time(&(u->stop));
////  printf("ran controller, start time: %.4f, end time: %.4f, diff time: %.3f us, time delay: %3.3fus\n",
////         t0,tf,(tf-t0)*1e6, (t0 - floating_time(&(y->imu.imu_accel_scaled.timestamp)))*1e6);
////}

void run_demo_controller(const Protobetty__Sensors* const y, Protobetty__Actuators* const u) {
    if (y->accel->data->y != 0.0)
        u->rudd = y->accel->data->y;
    if (y->accel->data->y != 0.0)
    {
        u->flaps = y->accel->data->x;
        u->elev = y->accel->data->x;
        u->ail =  -1*y->accel->data->x;
    }
    timestamp_t timestamp;
    gettime(&timestamp);
    u->start->tsec = timestamp.tsec;
    u->start->tnsec = timestamp.tnsec;

}
