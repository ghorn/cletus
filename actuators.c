#/* Copyright 2014 Greg Horn <gregmainland@gmail.com>
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
#include "./actuators.h"

//#include "./lisa_communication/data_decoding.h"

const double coef = 1.0;
const int LENGTH_INDEX = 1;

void convert_for_lisa(const Protobetty__Actuators * const actuators, lisa_message_t* const msg)
{
  msg->servos_msg.servo_1 = (int16_t)(actuators->ail * coef);
  msg->servos_msg.servo_2 = (int16_t)(actuators->ail * coef);
  msg->servos_msg.servo_3 = (int16_t)(actuators->flaps * coef);
  msg->servos_msg.servo_4= (int16_t)(actuators->flaps * coef);
  msg->servos_msg.servo_5 = (int16_t)(actuators->elev * coef);
  msg->servos_msg.servo_6 = (int16_t)(actuators->rudd * coef);
  msg->servos_msg.servo_7 = 0;

  calculate_checksum((uint8_t*) msg, &(msg->checksums.checksum1), &(msg->checksums.checksum2));
}


void calculate_checksum(uint8_t buffer[],uint8_t *checksum_1,uint8_t *checksum_2){
#ifdef DEBUG
  printf("Entering calculate_checksum\n");
#endif
  int length = buffer[LENGTH_INDEX];
  *checksum_1=0;
  *checksum_2=0;

  //start byte '0x99' is not in checksum
  for (int i=1;i<length-2;i++)
    {
      *checksum_1 += buffer[i];
      *checksum_2 += *checksum_1;
    }
}
