#include "lisa.h"
#include "ftdi_device.h"
#include "lisa_messages.h"

#include <stdlib.h>
#include <stdio.h>


#define LISA_BAUDRATE 921600
#define LISA_DEVICE 2
#define LISA_BIT_TYPE 8
#define LISA_PARITY 0
#define LISA_LATENCY 2
#define LISA_READ_CHUNKSIZE 512
#define LISA_WRITE_CHUNKSIZE 512


int lisa_read_data(unsigned char *buff, int n);
int lisa_process_message(int (*read)(unsigned char *buff, int n));


enum LISA_MSG_STATES{
    STARTBYTE = 1,
    MSG_LENGTH = 2,
    MSG_DATA = 3
};


enum LISA_RETURNS{
    WAITING = 0,
    MORE_DATA = 1,
    COMPLETE = 2,
    ERROR = -1
};

typedef struct {
    int read_state;
    int n_read;
    int msg_length;
} lisa_state_t;


typedef struct {
    lisa_state_t* state;
    ftdi_device ftdi;
    unsigned char* buffer;
} lisa_t;


lisa_t lisa;

int lisa_init_message_processing(unsigned char* buffer)
{
    lisa.buffer = buffer;
    lisa.state = malloc(sizeof(lisa_state_t));
    if (lisa.state == NULL)
        return -1;
    lisa.state->n_read = 0;
    lisa.state->read_state = STARTBYTE;
    lisa.state->msg_length = 0;
    return  0;
}


int lisa_flush_buffers(void)
{
    return flush_device(lisa.ftdi, BOTH);
}



int lisa_open_connection(void)
{
    int ret = 0;
    open_device(&lisa.ftdi,LISA_DEVICE, LISA_BAUDRATE, LISA_BIT_TYPE, LISA_PARITY);
    if (ret < 0 || lisa.ftdi == NULL)
    {
        printf("Error opening LISA device\n");
        return ret;
    }
    ret = set_chunksize(lisa.ftdi, LISA_READ_CHUNKSIZE, LISA_WRITE_CHUNKSIZE);
    if (ret < 0)
    {
        printf("Error setting LISA chunksize\n");
        return ret;
    }
    ret = set_latency_timer(lisa.ftdi, LISA_LATENCY);
    if (ret < 0)
    {
        printf("Error setting LISA latency");
        return ret;
    }
    flush_device(lisa.ftdi, BOTH);
    return ret;
}


void lisa_close_connection(void)
{
    int ret = close_device(lisa.ftdi);
    if (ret < 0 )
    {
        printf ("Error closing PIKSI device\n");
    }

}


int lisa_read_data(unsigned char *buff, int n){
    //printf("reading fifo thingy, length %d\n", n);
    return  read_data_from_device(lisa.ftdi, buff, n);

}

int lisa_process_message(int (*read)(unsigned char *buff, int n))
{
    switch(lisa.state->read_state)
    {
    case STARTBYTE:
        if ((*read)(lisa.buffer, 1) == 1)
        {
            if (lisa.buffer[0] == LISA_STARTBYTE) {
                lisa.state->n_read = 1;
                lisa.state->read_state = MSG_LENGTH;
                return MORE_DATA;
            }
        }
        return WAITING;
        break;
    case MSG_LENGTH:
        lisa.state->n_read += (*read)(&lisa.buffer[LISA_INDEX_MSG_LENGTH], 1);
        if (lisa.state->n_read == 2)
        {
           lisa.state->msg_length = lisa.buffer[LISA_INDEX_MSG_LENGTH];
           lisa.state->read_state = MSG_DATA;
           return MORE_DATA;
        }
        return WAITING;
        break;
    case MSG_DATA:
        lisa.state->n_read += (*read)(&lisa.buffer[LISA_INDEX_SENDER_ID],lisa.state->msg_length-lisa.state->n_read);
        if (lisa.state->n_read == lisa.state->msg_length)
        {
            lisa.state->read_state = STARTBYTE;
            //printf("Read message %d of %d", lisa.state->n_read, lisa.state->msg_length);
            return COMPLETE;
        }
        else if (lisa.state->n_read <  lisa.state->msg_length)
        {
            return MORE_DATA;
        }
        else
        {
            lisa.state->read_state = STARTBYTE;
            return ERROR;
        }
        break;

    default:
        printf("ERROR unknown message state LISA\n");
        break;
    }
    return ERROR;


}

int lisa_read_message(int* exit)
{
    int ret;
    do {
        ret = lisa_process_message(&lisa_read_data);
    //    printf("retval %d exit %d\n", ret, *exit);
        if (*exit) return ERROR;
    } while (ret == MORE_DATA);
    //printf("Message read!\n");
    return ret;
}



