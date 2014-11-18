/* Copyright 2014 Matt Peddie <peddie@alum.mit.edu>
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

#include "./zmq.h"
#include "./comms.h"
#include "./structures.h"
#include "./log.h"

#include "./uart.h"
#include "./lisa_messages.h"



void *zsock_print = NULL;


static void __attribute__((noreturn)) die(int code) {

  printf("Moriturus te saluto!\n");
  exit(code);
}

/* Our signal to GTFO */
static int bail = 0;

static void sigdie(int signum) {
  bail = signum;
}

int main(int argc __attribute__((unused)),
         char **argv __attribute__((unused))) {
  int value= 0;

  setbuf(stdin, NULL);
  /* Confignals. */
  if (signal(SIGINT, &sigdie) == SIG_IGN)
    signal(SIGINT, SIG_IGN);
  if (signal(SIGTERM, &sigdie) == SIG_IGN)
    signal(SIGTERM, SIG_IGN);
  if (signal(SIGHUP, &sigdie) == SIG_IGN)
    signal(SIGHUP, SIG_IGN);
  if (signal(SIGABRT, &sigdie) == SIG_IGN)
    signal(SIGABRT, SIG_IGN);

  int errRet = serial_port_setup();
  if(errRet!=UART_ERR_NONE){
      err("couldn't initialize serial port");
      die(SIGABRT);
    }
  sleep(2);

  lisa_actuators_t output;
  output.startbyte = 0x99;
  output.servos_msg.header.length = sizeof(lisa_actuators_t);
  output.servos_msg.header.sender_id = SENDER_ID;
  output.servos_msg.header.msg_id = SERVO_COMMANDS;


  for (value=0;value < 9600; value++) {
      if (bail) die(bail);

      output.servos_msg.servo_1 = value;
      output.servos_msg.servo_2 = value;
      output.servos_msg.servo_3 = value;
      output.servos_msg.servo_4 = value;
      output.servos_msg.servo_5 = value;
      output.servos_msg.servo_6 = value;
      output.servos_msg.servo_7 = value;

      write_uart((uint8_t*)&output,sizeof(output));

      usleep(5000);
    }

  for (value=0;value < 9600; value++) {
      if (bail) die(bail);

      output.servos_msg.servo_1 = -value;
      output.servos_msg.servo_2 = -value;
      output.servos_msg.servo_3 = -value;
      output.servos_msg.servo_4 = -value;
      output.servos_msg.servo_5 = -value;
      output.servos_msg.servo_6 = -value;
      output.servos_msg.servo_7 = -value;

      write_uart((uint8_t*)&output,sizeof(output));

      usleep(5000);
    }

  /* Shouldn't get here. */
  return 0;
}

