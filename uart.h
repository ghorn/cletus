#ifndef UART_H
#define UART_H

#include <sys/time.h>
#include <termios.h>
#include <unistd.h>
#include <stdint.h>
#include <inttypes.h>
#include <sys/ioctl.h>
#include <sys/signal.h>
#include "./lisa_communication/circular_buffer.h"



#include "./print_output.h"

#define INPUT_BUFFER_SIZE 255

/********************************
 * GLOBALS
 * ******************************/

extern void *zsock_print;



enum lisa_msg_info{
    BYTES_CHECKSUM = 2, //Number of bytes used for the checksum
    BYTES_HEADER =  2, //Number of Bytes until message  (length, senderid)
    SENDER_ID = 166, //Senser ID of the Lisa set with paparazzi
    LISA_STARTBYTE = 0x99,
    WINDSENSOR_STARTBYTE =0x24,
    LISA_INDEX_MSG_ID = 2,
    LISA_INDEX_SENDER_ID = 1,
    LISA_MAX_MSG_LENGTH = 50
};


enum uart_errCode {
    UART_ERR_READ= -6 ,
    UART_ERR_READ_START_BYTE = -5,
    UART_ERR_READ_CHECKSUM = -4,
    UART_ERR_READ_LENGTH = -3,
    UART_ERR_READ_MESSAGE = -2,
    UART_ERR_NONE=0,
    UART_ERR_SERIAL_PORT_FLUSH_INPUT,
    UART_ERR_SERIAL_PORT_FLUSH_OUTPUT,
    UART_ERR_SERIAL_PORT_OPEN,
    UART_ERR_SERIAL_PORT_CLOSE,
    UART_ERR_SERIAL_PORT_CREATE,
    UART_ERR_SERIAL_PORT_WRITE,
    UART_ERR_UNDEFINED
};
typedef enum uart_errCode UART_errCode;

enum uart_stages
{
    STARTBYTE_SEARCH,
    MESSAGE_LENGTH,
    MESSAGE_READING
};


typedef struct{
    int fd;                        /* serial device fd          */
    struct termios orig_termios;   /* saved tty state structure */
    struct termios cur_termios;    /* tty state structure       */
    struct sigaction saio;
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
extern int serial_input_get_lisa_data(uint8_t *const buffer); //returns the number of read bytes or a negative error message and puts the result in serial_input
extern int serial_input_get_windsensor_data(uint8_t buffer[]);
extern UART_errCode write_uart(uint8_t output[],long unsigned int message_length);
int read_uart(uint8_t* const buffer,int length);
extern UART_errCode serial_port_close(void);
int serial_port_read_temp(uint8_t buffer[],int length) ;
int check_checksum(const uint8_t * const message);
UART_errCode serial_port_flush_input(void);
int add_timestamp(uint8_t* const buffer, const int msg_length);
int read_lisa_message(zmq_pollitem_t* const pollitem, uint8_t* const buffer);
int set_global_variables(zmq_pollitem_t* const pollitem, uint8_t* const msg_buffer);

void init_circular_buffer(int buffer_size);


extern CircularBuffer cb;




extern void UART_err_handler( UART_errCode err_p,void (*write_error_ptr)(char *,char *,int));




#endif // UART_H
