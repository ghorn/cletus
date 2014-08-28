#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <inttypes.h>
#include <poll.h>


#include "./misc.h"
#include "./structures.h"
#include "./uart.h"


UART_errCode serial_port_new(void);
UART_errCode serial_port_create(void);
UART_errCode  serial_port_open_raw(const char* device_ptr, speed_t speed_param, int signal_hanlder);
void serial_port_free(void);
void serial_port_flush(void);
UART_errCode serial_port_flush_output(void);
void signal_handler_IO (int status);


static uint8_t irq_msg_length;
static int irq_readbytes;
static uint8_t irq_msg_buffer[256];
static uint8_t* zmq_buffer;
static zmq_pollitem_t* poll_lisa;
static ElemType buffer_element;
CircularBuffer cb;

static int uart_stage;
static char FILENAME[] = "uart_communication.c";

//extern serial_port *serial_stream;

speed_t speed = B57600;
//Variables for serial port
const char device[]="/dev/ttyO4";
const char device_enabled_check[] = "ttyO4_armhf.com"; //For Angstrom: enable-uart5
const char device_path[] = "/sys/devices/bone_capemgr.9/slots"; //For Angstrom: /sys/devices/bone_capemgr.8/slots


int add_timestamp(uint8_t*const buffer,const int msg_length)
{
    timestamp_t timestamp;
    gettime(&timestamp);
#ifdef DEBUG
    printf("Added timestamp: %f\n", floating_time(&timestamp));
#endif
    int timestamp_position = msg_length - BYTES_CHECKSUM -1;
    memcpy(&buffer[timestamp_position],&timestamp,sizeof(timestamp));
    return timestamp_position + sizeof(timestamp);
}


int read_uart(uint8_t* const buffer,int length)
{
#ifdef DEBUG
    //    printf("Entering serial_port_read\n");
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

int check_checksum(const uint8_t * const message)
{
    uint8_t length = message[0];
    uint8_t checksum_1 = 0;
    uint8_t checksum_2 = 0;

    int INDEX_CH1 = length-2-1; //2 Checksum bytes - 1 Startbyte
    int INDEX_CH2 = length-1-1; //1 Checksum bytes - 1 Startbyte

    for(int i=0;i<length-2-1;i++) //read until message_length - checksum_1 - checksum_2 - startbyte
    {
        checksum_1 += message[i];
        checksum_2 += checksum_1;
    }

    if (message[INDEX_CH1]!= checksum_1 || message[INDEX_CH2] != checksum_2)
    {
#ifdef DEBUG
        printf(" Checksum error: message raw check: ");
        for(int i=0;i<length-1;i++){
            printf("%d ",message[i]);
        }
        printf("\n");
#endif
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

UART_errCode serial_port_setup(int signal_handler_flag)
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

    err = serial_port_open_raw(device, speed, signal_handler_flag);
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

UART_errCode  serial_port_open_raw(const char* device_ptr, speed_t speed_param, int signal_hanlder) {

#ifdef DEBUG
    printf("Entering serial_port_open_raw\n");
#endif
    if((serial_stream->fd = open(device_ptr, O_RDWR | O_NOCTTY | O_NDELAY)) < 0)
        return UART_ERR_SERIAL_PORT_OPEN;

    sigset_t block_mask;
    sigemptyset (&block_mask);
    serial_stream->saio.sa_handler = signal_handler_IO;
    serial_stream->saio.sa_flags = 0;
    serial_stream->saio.sa_restorer = NULL;
    sigaction(SIGIO,&(serial_stream->saio),NULL);


    fcntl(serial_stream->fd, F_SETFL, FNDELAY);
    if (signal_hanlder > 0)
        fcntl(serial_stream->fd, F_SETOWN, getpid());
    else {
        pid_t child = fork();
        fcntl(serial_stream->fd, F_SETOWN, child);
    }
    fcntl(serial_stream->fd, F_SETFL, FASYNC);


    serial_stream->cur_termios = serial_stream->orig_termios;
    tcgetattr(serial_stream->fd,&(serial_stream->cur_termios));
    cfsetispeed(&(serial_stream->cur_termios),speed_param);
    cfsetospeed(&(serial_stream->cur_termios),speed_param);
    serial_stream->cur_termios.c_cflag &= ~PARENB;
    serial_stream->cur_termios.c_cflag &= ~CSTOPB;
    serial_stream->cur_termios.c_cflag &= ~CSIZE;
    serial_stream->cur_termios.c_cflag |= CS8;
    serial_stream->cur_termios.c_cflag |= (CLOCAL | CREAD);
    serial_stream->cur_termios.c_lflag &= ~(ICANON | ECHO | ECHOE | ISIG);
    serial_stream->cur_termios.c_iflag &= ~(IXON | IXOFF | IXANY);
    serial_stream->cur_termios.c_oflag &= ~OPOST;
    tcsetattr(serial_stream->fd,TCSANOW,&(serial_stream->cur_termios));
    printf("UART4 configured....\n");

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




int set_global_variables(zmq_pollitem_t* const pollitem, uint8_t* const msg_buffer)
{
    poll_lisa = pollitem;
    zmq_buffer = msg_buffer;
    return 1;
}





void signal_handler_IO (int status)
{
    if (status == SIGIO)
    {
        switch (uart_stage) {
        case STARTBYTE_SEARCH:
            irq_readbytes = 0;
            ioctl(serial_stream->fd, FIONREAD,&irq_readbytes); //set to number of bytes in buffer
            read_uart(irq_msg_buffer,1);
            if (irq_msg_buffer[0] == LISA_STARTBYTE)
            {
                uart_stage = MESSAGE_LENGTH;
            }
            break;
        case MESSAGE_LENGTH:
            ioctl(serial_stream->fd, FIONREAD,&irq_readbytes); //set to number of bytes in buffer
            read_uart(buffer_element.message,1);
            irq_msg_length = buffer_element.message[0];
            if (irq_msg_length < LISA_MAX_MSG_LENGTH)
                uart_stage = MESSAGE_READING;
            else
                uart_stage = STARTBYTE_SEARCH;
            break;
        case MESSAGE_READING:
            ioctl(serial_stream->fd, FIONREAD, &irq_readbytes); //set to number of bytes in buffer
            if (!(irq_readbytes < irq_msg_length))
            {
                read_uart(&(buffer_element.message[1]),irq_msg_length-2);
#ifdef DEBUG
                //                printf("Received message ");
                //                for (int i = 0; i < irq_msg_length-1; i++)
                //                {
                //                    printf(" %i ", buffer_element.message[i]);
                //                }
                //                printf("\n");
#endif
                if (cb.elems != NULL)
                    cbWrite(&cb,&buffer_element);
                uart_stage = STARTBYTE_SEARCH;
            }
            break;
        default:
            break;
        }
    }
}




