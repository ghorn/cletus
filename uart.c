#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <inttypes.h>



#include "./uart.h"
#include <time.h>


UART_errCode serial_port_new(void);
UART_errCode serial_port_create(void);
UART_errCode  serial_port_open_raw(const char* device_ptr, speed_t speed_param);
void serial_port_free(void);
void serial_port_flush(void);
UART_errCode serial_port_flush_output(void);



static char FILENAME[] = "uart_communication.c";

//extern serial_port *serial_stream;

speed_t speed = B921600;
//Variables for serial port
const char device[]="/dev/ttyO4";
const char device_enabled_check[] = "ttyO4_armhf.com"; //For Angstrom: enable-uart5
const char device_path[] = "/sys/devices/bone_capemgr.9/slots"; //For Angstrom: /sys/devices/bone_capemgr.8/slots


int add_timestamp(uint8_t*const buffer,const int msg_length){
#if DEBUG  > 1
  printf("Entering add_timestamp\n");
#endif

  struct timespec timestamp;
  clock_gettime(CLOCK_MONOTONIC, &timestamp);
  int timestamp_position = msg_length - BYTES_HEADER - BYTES_CHECKSUM;
  memcpy(&buffer[timestamp_position],&timestamp,sizeof(timestamp));

  return timestamp_position + sizeof(timestamp) +1;
}


int read_uart(uint8_t* const buffer,int length)
{
#ifdef DEBUG
  printf("Entering serial_port_read\n");
#endif
  int n = read(serial_stream->fd, buffer, length);

  if(n==-1){
      return UART_ERR_READ;
    }

  return n;  //return number of read bytes
}


UART_errCode write_uart(uint8_t output[],long unsigned int message_length)
{
#ifdef DEBUG
  printf("Entering serial_port_write\n");
#endif

  int n = write(serial_stream->fd, output, message_length);

  if (n < 0)
    {
      return UART_ERR_SERIAL_PORT_WRITE;
    }
  return UART_ERR_NONE;
}

int check_checksum(uint8_t length, uint8_t *message)
{
  uint8_t checksum_1 = length;
  uint8_t checksum_2 = checksum_1;

  int INDEX_CH1 = length-2-2;
  int INDEX_CH2 = length-1-2;

  for(int i=0;i<length-2-2;i++) //read until message_length - checksum_1 - checksum_2 - start &length byte
    {
      checksum_1 += message[i];
      checksum_2 += checksum_1;
    }

  if (message[INDEX_CH1]!= checksum_1 || message[INDEX_CH2] != checksum_2)
    {
#ifdef DEBUG
      printf(" Checksum error: message raw check: ");
      for(int i=0;i<length-2;i++){
          printf("%d ",message[i]);
        }
      printf("\n");
#endif
      serial_port_flush_input();
      return UART_ERR_READ_CHECKSUM;

    }
  return UART_ERR_NONE;
}


void serial_port_free(void) {
#ifdef DEBUG
  printf("Entering serial_port_free\n");
#endif

  free(serial_stream);
}

void serial_port_flush(void) {
#ifdef DEBUG
  printf("Entering serial_port_flush\n");
#endif
  /*
         * flush any input and output on the port
         */
  serial_port_flush_input();
  serial_port_flush_output();
}


UART_errCode serial_port_flush_input(void) {
#ifdef DEBUG
  printf("Entering serial_port_flush_input\n");
#endif
  /*
         * flush any input that might be on the port so we start fresh.
         */
  if (tcflush(serial_stream->fd, TCIFLUSH)) {
      return UART_ERR_SERIAL_PORT_FLUSH_INPUT;
    }
  return UART_ERR_NONE;
}

UART_errCode serial_port_flush_output(void) {

#ifdef DEBUG
  printf("Entering serial_port_flush_output\n");
#endif

  /*
         * flush any input that might be on the port so we start fresh.
         */
  if (tcflush(serial_stream->fd, TCOFLUSH)) {
      return UART_ERR_SERIAL_PORT_FLUSH_OUTPUT;

    }
  return UART_ERR_NONE;
}



//FUNCTIONS FOR UART SETUP

UART_errCode serial_port_setup(void)
{
#ifdef DEBUG
  printf("Entering serial_port_setup\n");
#endif

  int err;

  err = serial_port_new();
  if(err!=UART_ERR_NONE){
      return err;
    }

  err = serial_port_create();
  if(err!=UART_ERR_NONE){
      return err;
    }

  err = serial_port_open_raw(device, speed);
  if(err!=UART_ERR_NONE){
      return err;
    }

  return UART_ERR_NONE;
}

UART_errCode serial_port_new(void) {

#ifdef DEBUG
  printf("Entering serial_port_new\n");
#endif

  serial_stream = (serial_port*) malloc(sizeof(serial_port));

  if(serial_stream==NULL){
      return UART_ERR_SERIAL_PORT_CREATE;
    }

  return UART_ERR_NONE;
}

UART_errCode serial_port_create(void)
{
#ifdef DEBUG
  printf("Entering serial_port_create\n");
#endif

  char  tmp[256]={0x0};
  char flag = 0;
  FILE *fp ;
  int fd;

  fp = fopen(device_path, "r");
  if (fp == NULL){

      return UART_ERR_SERIAL_PORT_CREATE;
    }

  //search enable-uart5 is present int the file
  while(flag!=1 && fp!=NULL && fgets(tmp, sizeof(tmp), fp)!=NULL)
    {
      if (strstr(tmp, device_enabled_check))
        {
          flag = 1;
        }
    }

#ifdef DEBUG

  if(flag)
    {
      printf("Uart5 is enabled\n");
    } else {
      printf("Uart5 is disabled\n");
    }
#endif

  fclose(fp);

  if (flag)
    {
      return UART_ERR_NONE;
    } else {
      fd = open(device_path, O_RDWR);

#ifdef DEBUG
      printf("Uart not enabled, trying to enable...\n");
#endif
      char command[100]= "echo ";
      strcat(command, device_enabled_check);
      strcat(command, " > ");
      strcat(command, device_path);
      if (system(command)==0)
        {
          close(fd);
          return UART_ERR_NONE;
        } else {
          close(fd);
          return UART_ERR_SERIAL_PORT_CREATE;
        }
    }
  return UART_ERR_NONE;
}

UART_errCode  serial_port_open_raw(const char* device_ptr, speed_t speed_param) {

#ifdef DEBUG
  printf("Entering serial_port_open_raw\n");
#endif

  if ((serial_stream->fd = open(device_ptr, O_RDWR | O_NONBLOCK | O_NOCTTY)) < 0) {
      return UART_ERR_SERIAL_PORT_OPEN;
    }
  if (tcgetattr(serial_stream->fd, &serial_stream->orig_termios) < 0) {
      close(serial_stream->fd);
      return UART_ERR_SERIAL_PORT_OPEN;
    }
  serial_stream->cur_termios = serial_stream->orig_termios;
  /* input modes  */
  serial_stream->cur_termios.c_iflag &= ~(IGNBRK|BRKINT|PARMRK|INPCK|ISTRIP|INLCR|IGNCR
                                          |ICRNL |IUCLC|IXON|IXANY|IXOFF|IMAXBEL);
  serial_stream->cur_termios.c_iflag |= IGNPAR;
  /* control modes*/
  serial_stream->cur_termios.c_cflag &= ~(CSIZE|PARENB|CRTSCTS|PARODD|HUPCL|CSTOPB);
  serial_stream->cur_termios.c_cflag |= CREAD|CS8|CLOCAL;
  /* local modes  */
  serial_stream->cur_termios.c_lflag &= ~(ISIG|ICANON|IEXTEN|ECHO|FLUSHO|PENDIN);
  serial_stream->cur_termios.c_lflag |= NOFLSH;
  if (cfsetispeed(&serial_stream->cur_termios, speed_param)) {
      close(serial_stream->fd);
      return UART_ERR_SERIAL_PORT_OPEN;
    }
  if (tcsetattr(serial_stream->fd, TCSADRAIN, &serial_stream->cur_termios)) {
      close(serial_stream->fd);
      return UART_ERR_SERIAL_PORT_OPEN;
    }
  serial_port_flush();
  return UART_ERR_NONE;
}

UART_errCode serial_port_close(void) {

#ifdef DEBUG
  printf("Entering serial_port_close\n");
#endif

  /* if null pointer or file descriptor indicates error just bail */
  if (!serial_stream || serial_stream->fd < 0)
    return UART_ERR_SERIAL_PORT_CLOSE;
  if (tcflush(serial_stream->fd, TCIOFLUSH)) {
      close(serial_stream->fd);
      return UART_ERR_SERIAL_PORT_CLOSE;
    }
  if (tcsetattr(serial_stream->fd, TCSADRAIN, &serial_stream->orig_termios)) {        // Restore modes.
      close(serial_stream->fd);
      return UART_ERR_SERIAL_PORT_CLOSE;
    }
  if (close(serial_stream->fd)) {
      return UART_ERR_SERIAL_PORT_CLOSE;
    }

  serial_port_free();

  return UART_ERR_NONE;

}

void UART_err_handler( UART_errCode err_p,void (*write_error_ptr)(char *,char *,int))
{
#ifdef DEBUG
  printf("Entering UART_err_handler\n");
#endif

  int8_t err = (int8_t)err_p; //because uart erros can be negative

  switch( err ) {
    case UART_ERR_NONE:
      break;
    case  UART_ERR_READ_START_BYTE:
      write_error_ptr(FILENAME,"serial port failed to read start byte",err);
      break;
    case  UART_ERR_READ_CHECKSUM:
      write_error_ptr(FILENAME,"serial port wrong checksum",err);
      break;
    case  UART_ERR_READ_LENGTH:
      write_error_ptr(FILENAME,"serial port failed reading message length",err);
      break;
    case  UART_ERR_READ_MESSAGE:
      write_error_ptr(FILENAME,"serial port failed reading message based on length",err);
      break;
    case UART_ERR_SERIAL_PORT_FLUSH_INPUT:
      write_error_ptr(FILENAME,"serial port flush input failed",err);
      break;
    case UART_ERR_SERIAL_PORT_FLUSH_OUTPUT:
      write_error_ptr(FILENAME,"serial port flush output failed",err);
      break;
    case UART_ERR_SERIAL_PORT_OPEN:
      write_error_ptr(FILENAME,"serial port open failed",err);
      break;
    case UART_ERR_SERIAL_PORT_CLOSE:
      write_error_ptr(FILENAME,"serial port close failed",err);
      break;
    case UART_ERR_SERIAL_PORT_CREATE:
      write_error_ptr(FILENAME,"serial port create failed",err);
      break;
    case UART_ERR_SERIAL_PORT_WRITE:
      write_error_ptr(FILENAME,"serial port write failed",err);
      break;
    case UART_ERR_UNDEFINED:
      write_error_ptr(FILENAME,"undefined UART error",err);
      break;
    default: break;
    }
}

