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
    if (y->imu->accel->y != 0.0)
        u->rudd = y->imu->accel->y;
    if (y->imu->accel->y != 0.0)
    {
        u->flaps = y->imu->accel->x;
        u->elev = y->imu->accel->x;
        u->ail =  -1*y->imu->accel->x;
    }
    Protobetty__Timestamp timestamp;
    get_protbetty_timestamp(&timestamp);
    u->timestamp = &timestamp;

}

int Kp = 2500;
int Kd = 75;
int Kdp = 50;
int Kas = 200;

void init_controller(int pKp, int pKd, int pKpd, int pKas)
{
    Kp = pKp;
    Kd = pKd;
    Kdp = pKpd;
    Kas = pKas;
}

void run_pd_demo_controller(const Protobetty__Sensors* const y, Protobetty__Actuators* const u)
{

    static double rudd_p = 0.0;
    if ((y->imu->accel->y != 0.0) && (y->imu->gyro->y !=0.0)){

        u->elev = -1*y->imu->accel->y *Kp + y->imu->accel->x*Kd;
        u->ail =y->imu->accel->x*Kp + y->imu->gyro->y*Kd;
        if (y->airspeed != NULL)
        {
            u->flaps = y->airspeed->scaled*(-1)*Kas;
        }

        rudd_p +=  y->imu->gyro->z*Kdp;
        u->rudd = rudd_p + y->imu->gyro->z*Kd;
    }
}
