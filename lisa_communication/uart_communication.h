/*
 * AUTHOR: Maarten Arits and Jonas Van Pelt
 */
 
/**************************************************************************************
* LAYOUT OF INCOMING LISA PACKAGE 
* startbyte (0x99) - length - sender_id, message_id, message ... , checksumA, checksumB
****************************************************************************************/

/****************************************************************************************************************************************************************************************************************************
* LAYOUT OF INCOMING WINDSENSOR PACAKGE
* startbyte ($) - talker_id_1 - talker_id_2, message_type_1, message_type_2, message_type_3, ... comma-delimited data .... , asterisk (if checksum follows) , checksum(if provided), <CR> , < LF > 
*
* Note:
* The checksum is the bitwise exclusive OR of ASCII codes of all characters between the $ and 
* <CR> , < LF >  ends the message
*
******************************************************************************************************************************************************************************************************************************/


#ifndef UART_COMMUNCATION_H_ 
#define UART_COMMUNCATION_H_

#include <sys/time.h>
#include <termios.h>
#include <unistd.h>
#include <stdint.h>

#define INPUT_BUFFER_SIZE 255 

/********************************
 * GLOBALS
 * ******************************/
 
enum uart_errCode {UART_ERR_READ= -6 ,UART_ERR_READ_START_BYTE = -5,UART_ERR_READ_CHECKSUM = -4,UART_ERR_READ_LENGTH = -3,UART_ERR_READ_MESSAGE = -2, UART_ERR_NONE=0,UART_ERR_SERIAL_PORT_FLUSH_INPUT,UART_ERR_SERIAL_PORT_FLUSH_OUTPUT,UART_ERR_SERIAL_PORT_OPEN,UART_ERR_SERIAL_PORT_CLOSE,UART_ERR_SERIAL_PORT_CREATE,UART_ERR_SERIAL_PORT_WRITE,UART_ERR_UNDEFINED};
typedef enum uart_errCode UART_errCode;
 
typedef struct{
	int fd;                        /* serial device fd          */
	struct termios orig_termios;   /* saved tty state structure */
	struct termios cur_termios;    /* tty state structure       */
}serial_port; 
 
serial_port *serial_stream;

/*typedef struct{
	uint8_t startbyte;
	uint8_t talker_id_1;
	uint8_t talker_id_2;
	uint8_t message_type_1;
	uint8_t message_type_2;
	uint8_t message_type_3;
	uint8_t *message;
	uint8_t asterisk;
	uint8_t checksum;
}NMEA0183_message;

typedef struct{
	uint8_t startbyte;
	uint8_t length;
	uint8_t sender_id;
	uint8_t message_id;
	uint8_t *message;
	uint8_t checksum_1;
	uint8_t checksum_2;
}Lisa_message;*/

/********************************
 * PROTOTYPES PUBLIC
 * ******************************/
 

 
extern UART_errCode serial_port_setup(void); 
extern int serial_input_get_lisa_data(uint8_t buffer[]); //returns the number of read bytes or a negative error message and puts the result in serial_input
extern int serial_input_get_windsensor_data(uint8_t buffer[]);
extern UART_errCode serial_port_write(uint8_t output[],long unsigned int message_length);
extern UART_errCode serial_port_close(void);

extern void UART_err_handler( UART_errCode err_p,void (*write_error_ptr)(char *,char *,int));



#endif /*UART_COMMUNCATION_H_*/
