#ifndef FTDI_DEVICE_H
#define FTDI_DEVICE_H
#include "ftdi_device.h"



typedef void* ftdi_device;

enum buffers
{
    BOTH = 0,
    RX = 1,
    TX = 2
};





int open_device(ftdi_device* ftdi_loc, const int device,const int baudrate, const int bits_type, const int parity);



int set_chunksize(ftdi_device ftdi, const unsigned int read_size, const unsigned int write_size);



int set_latency_timer(ftdi_device ftdi, unsigned char latency_ms);

int flush_device(ftdi_device ftdi, const int buffer);



int close_device(ftdi_device ftdi);


int read_data_from_device(ftdi_device ftdi, unsigned char *buffer, int size);


int write_data_to_device(ftdi_device ftdi,const unsigned char* buffer, const int size);



#endif // FTDI_DEVICE_H
