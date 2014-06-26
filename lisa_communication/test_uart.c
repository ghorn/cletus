//This program is for testing the speed of the UART port
//UART settings have to made in UART_communication.c/.h
//The Lisa should be configured to send only the IMU_ACCEL_RAW message


#include "./uart_communication.h"
#include "../structures.h"
#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <signal.h>


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
      printf("couldn't initialize serial port \n");
      die(SIGABRT);
    }


  uint8_t buffer[INPUT_BUFFER_SIZE];
  double time_spent;
  clock_t begin, end;
  begin = clock();
  for (;;){

      if (bail) die(bail);

      int message_length = serial_input_get_lisa_data(buffer);

      if (message_length == sizeof(xyz_int) + 2)
        {
          end = clock();
          time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
          printf("\rReading \t%f \t bytes/sec \n Receiving \t %f \t packages/sec \n", message_length/time_spent, 1.0/time_spent);
          begin = clock();
        }
      else{
#ifdef DEBUG
          printf("Wrong message length %i instead of %i", message_length,(int)(sizeof(xyz_int) + 2));
    #endif
        }
    }







}
