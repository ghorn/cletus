#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <time.h>
#include <poll.h>

#include "uart_communication.h"

#ifndef DEBUG 
#define DEBUG 1
#endif

int serial_port_read(uint8_t buffer[],int length); 
UART_errCode serial_port_new(void);
UART_errCode serial_port_create();
UART_errCode  serial_port_open_raw(const char* device, speed_t speed);
void serial_port_free(void);
void serial_port_flush(void);
UART_errCode serial_port_flush_input(void);
UART_errCode serial_port_flush_output(void);
int wait_for_data();



static char FILENAME[] = "uart_communication.c";

extern serial_port *serial_stream;

speed_t speed = B921600;
//Variables for serial port
const char device[]="/dev/ttyO4";
const char device_enabled_check[] = "ttyO4_armhf.com"; //For Angstrom: enable-uart5
const char device_path[] = "/sys/devices/bone_capemgr.9/slots"; //For Angstrom: /sys/devices/bone_capemgr.8/slots


int wait_for_data(){
	struct pollfd fds[1];
	int timeout = -1; //infinite timeout
	int result;
	fds[0].fd=serial_stream->fd;
	fds[0].events=POLLIN;
	result=poll(fds,1,timeout); //block until there is data in the serial stream

	if((result & (1 << 0)) == 0){
		return -1;	
	}
	return 0;
}

int serial_port_read(uint8_t buffer[],int length) 
{
	#if DEBUG  > 1
		printf("Entering serial_port_read\n");
	#endif
	
	int bytes_in_buffer=0;

	do{
		wait_for_data();
		ioctl(serial_stream->fd, FIONREAD, &bytes_in_buffer); //set to number of bytes in buffer
		//printf("bytes in buff %d\n",bytes_in_buffer);
	
	}while(bytes_in_buffer < length );
	 
	int n = read(serial_stream->fd, buffer, length);
	 
	if(n==-1){
		return UART_ERR_READ;
	}  
	    
	return n;  //return number of read bytes
}

int serial_input_get_lisa_data(uint8_t buffer[]){
	
	#if DEBUG  > 1
		printf("Entering serial_input_get_lisa_data\n");
	#endif
	
	//todo: oneindige loops vermijden
	
	uint8_t message_length;
	uint8_t checksum_1=0;
	uint8_t checksum_2=0;
	int INDEX_CH1,INDEX_CH2;

	//1. SEARCH FOR START BYTE 0X99
	do{
		if(serial_port_read(&buffer[0],1)==UART_ERR_READ){	//read first byte
			serial_port_flush_input();
			return UART_ERR_READ_START_BYTE;
		} 
	
	}while(buffer[0]!=0x99);

	//buffer[0] = 0x99 at this moment
	
	//2. READ MESSAGE LENGTH
	if(serial_port_read(&buffer[1],1)==UART_ERR_READ){	//read first byte
			serial_port_flush_input();
			return UART_ERR_READ_LENGTH;
	} 
	message_length = buffer[1]; 
	//buffer[1] = length at this moment
	
	//3. READ MESSAGE
	if(serial_port_read(&buffer[2],message_length-2)==UART_ERR_READ){	//read only message_length -2 because start byte and length are already in there
		serial_port_flush_input();			
		return UART_ERR_READ_MESSAGE;
	} 
	
	//4 CHECK CHECKSUMS
	INDEX_CH1 = message_length-2;
	INDEX_CH2 = message_length-1;
	
	for(int i=1;i<message_length-2;i++) //read until message_length - checksum_1 - checksum_2
	{
		checksum_1 += buffer[i];
		checksum_2 += checksum_1;
	}
	
	if (buffer[INDEX_CH1]!= checksum_1 || buffer[INDEX_CH2] != checksum_2)
	{
		/*printf("message raw check: ");
		for(i=0;i<message_length;i++){
				printf("%d ",buffer[i]);
		}
		printf("\n");*/
		serial_port_flush_input();
		return UART_ERR_READ_CHECKSUM; 
		
	}
	
	/*printf("message raw: ");
	for(i=0;i<message_length;i++){
			printf("%d ",buffer[i]);
	}
	printf("\n");*/
	
	return message_length;
}

int serial_port_read_temp(uint8_t buffer[],int length) 
{
	const char test_wind[] = "$IIMWV,226.0,R,000.00,N,A*0B\n$WIXDR,C,036.5,C,,*52\n";
	static int index=0;
	//printf("reading index %d ...\n",index);

	buffer[0]=(uint8_t)test_wind[index];
	index++;
	
	if(index==sizeof(test_wind)){
			exit(1);
	}
	
	return 1;  //return number of read bytes
}

int serial_input_get_windsensor_data(uint8_t buffer[]){	
	
	#if DEBUG  > 1
		printf("Entering serial_input_get_windsensor_data\n");
	#endif
	
	//Naser: Replace serial_port_read_temp with serial_port_read
	
		//check protocol here: http://en.wikipedia.org/wiki/NMEA_0183
		unsigned int checksum_calc=0;
		unsigned int checksum_recv=0;
		uint8_t checksum_flag=0;
		int length,i;
		
		//1. SEARCH FOR START BYTE $ = 0x24
		do{
			if(serial_port_read_temp(&buffer[0],1)==UART_ERR_READ){	//read first byte
				serial_port_flush_input();
				return UART_ERR_READ_START_BYTE;
			} 

		}while(buffer[0]!='$');
		//buffer[0] = 0x24 at this moment	
					
		//2. READ THE REST OF THE MESSAGE UNTIL <CR> = 0x0d < LF > = 0x0a
		length = 0;
		do{
			length++;
			if(serial_port_read_temp(&buffer[length],1)==UART_ERR_READ){	
				serial_port_flush_input();			
				return UART_ERR_READ_MESSAGE;
			}

			if(buffer[length]=='*'){ //0x0a = '*' --> if present then there is a checksum
					//printf("checksum present at index %d\n",length);
					checksum_flag=1;
			}
		
		}while(buffer[length]!='\n'); //Naser: maybe on real uart message does not end with '\n'
		
		//3.CHECK CHECKSUM IF PRESENT
		//the checksum is the bitwise exclusive OR of ASCII codes of all characters between the $ and *
		if(checksum_flag==1){
			
			sscanf((const char *) &buffer[length-2],"%x",(unsigned int *)&checksum_recv); //convert fake asci hex to real hex

			for(i=1;i<length-3;i++){
				checksum_calc ^= (char)buffer[i];
			}
						
			if(checksum_calc!=checksum_recv){
				serial_port_flush_input();	
				//printf("checksum calc %x\n",checksum_calc);
				//printf("checksum real %x\n",checksum_recv);
				return UART_ERR_READ_CHECKSUM; 
			}	
		}	
		
		return length; //number of read bytes
}


UART_errCode serial_port_new(void) {
	
	#if DEBUG  > 1
		printf("Entering serial_port_new\n");
	#endif
	
	serial_stream = (serial_port*) malloc(sizeof(serial_port));
	
	if(serial_stream==NULL){
			return UART_ERR_SERIAL_PORT_CREATE;
	}
	
	return UART_ERR_NONE;
}

void serial_port_free(void) {
	#if DEBUG  > 1
		printf("Entering serial_port_free\n");
	#endif
	
	free(serial_stream);
}

void serial_port_flush(void) {
	#if DEBUG  > 1
		printf("Entering serial_port_flush\n");
	#endif
	/*
	 * flush any input and output on the port
	 */
	serial_port_flush_input();
	serial_port_flush_output();
}


UART_errCode serial_port_flush_input(void) {
	#if DEBUG  > 1
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
	
	#if DEBUG  > 1
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

UART_errCode  serial_port_open_raw(const char* device, speed_t speed) {
	
	#if DEBUG  > 1
		printf("Entering serial_port_open_raw\n");
	#endif
	
	if ((serial_stream->fd = open(device, O_RDWR | O_NONBLOCK | O_NOCTTY)) < 0) {
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
	if (cfsetispeed(&serial_stream->cur_termios, speed)) {
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
	
	#if DEBUG  > 1
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



UART_errCode serial_port_setup(void)
{
	#if DEBUG  > 1
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

UART_errCode serial_port_create()
{
	#if DEBUG  > 1
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
	
	#if DEBUG
	
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
		
		#if DEBUG
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


UART_errCode serial_port_write(uint8_t output[],long unsigned int message_length) 
{
	#if DEBUG  > 1
		printf("Entering serial_port_write\n");
	#endif

	int n = write(serial_stream->fd, output, message_length);
		
	if (n < 0) 
	{
		return UART_ERR_SERIAL_PORT_WRITE;
	}
	return UART_ERR_NONE;                                                                                                           
}

void UART_err_handler( UART_errCode err_p,void (*write_error_ptr)(char *,char *,int))
{
	#if DEBUG  > 1
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
