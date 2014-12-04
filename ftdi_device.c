#include "ftdi_device.h"
#include <stdio.h>
#include <stdlib.h>
#include "libftdi/src/ftdi.h"

typedef struct ftdi_context* ftdi_ptr;

#define VID_DEFAULT 0x0403
#define PID_DEFAULT 0x6011

#define RETURN_ERROR -1

int open_device(ftdi_device* ftdi_loc, const int device,const int baudrate, const int bits_type __attribute__((unused)), const int parity __attribute__((unused)))
{
    ftdi_device ftdi;
    int ret;
    // Init
    if ((ftdi = ftdi_new()) == 0)
    {
        fprintf(stderr, "ftdi_new failed\n");
        return RETURN_ERROR;
    }
    *ftdi_loc = ftdi;
    if (ftdi == NULL)
    {
        fprintf(stderr, "ftdi_new failed\n");
        return RETURN_ERROR;
    }
    //set interface
    ret = ftdi_set_interface(ftdi, device);
    if (ret < 0)
    {
        fprintf(stderr, "ftdi_set_interface failed\n");
        return RETURN_ERROR;
    }
    // Open device
    ret = ftdi_usb_open(ftdi, VID_DEFAULT, PID_DEFAULT);
    if (ret < 0)
    {
        fprintf(stderr, "unable to open ftdi device: %d (%s)\n", ret, ftdi_get_error_string(ftdi));
        exit(-1);
    }
    // Set baudrate
    ret = ftdi_set_baudrate(ftdi, baudrate);
    if (ret < 0)
    {
        fprintf(stderr, "unable to set baudrate: %d (%s)\n", ret, ftdi_get_error_string(ftdi));
        return RETURN_ERROR;
    }

    ret = ftdi_set_line_property(ftdi, BITS_8, STOP_BIT_1, NONE );
    if (ret < 0)
    {
        fprintf(stderr, "unable to set line parameters: %d (%s)\n", ret, ftdi_get_error_string(ftdi));
        return RETURN_ERROR;
    }
    ret = ftdi_disable_bitbang(ftdi);
    if (ret < 0)
    {
        fprintf(stderr, "ftdi_disable_bitbang failed\n");
        return RETURN_ERROR;
    }
    ret = ftdi_setflowctrl(ftdi, SIO_DISABLE_FLOW_CTRL);
    if (ret < 0)
    {
        fprintf(stderr, "ftdi_setflowctrl failed\n");
        return RETURN_ERROR;
    }
    ftdi_set_error_char(ftdi, 0,0);
    ftdi_set_event_char(ftdi, 0, 0);


    return ret;

}


int set_chunksize(ftdi_device ftdi, const unsigned int read_size, const unsigned int write_size)
{
    int ret;
    unsigned int check;
    ret = ftdi_read_data_set_chunksize((ftdi_ptr)ftdi, read_size);
    ret = ftdi_read_data_get_chunksize((ftdi_ptr)ftdi, &check) ;
    if (check!= read_size)
    {
        fprintf(stderr, "Setting read chunksize to %u failed\n", read_size);
        return RETURN_ERROR;
    }
    ret = ftdi_write_data_set_chunksize((ftdi_ptr)ftdi, read_size);
    ret = ftdi_write_data_get_chunksize((ftdi_ptr)ftdi, &check) ;
    if (check!= write_size)    {
        fprintf(stderr, "Setting write chunksize to %u failed\n", write_size);
        return RETURN_ERROR;
    }
    return ret;
}


int set_latency_timer(ftdi_device ftdi, unsigned char latency_ms)
{
    int ret;
    unsigned char check;
    ftdi_set_latency_timer((ftdi_ptr)ftdi, latency_ms);
    ret = ftdi_get_latency_timer(ftdi, &check) ;
    if (check != latency_ms)
    {
        fprintf(stderr, "Setting latency timer to %u ms failed\n", latency_ms);
        return RETURN_ERROR;
    }
    return ret;
}


int flush_device(ftdi_device ftdi, const int buffer)
{
    int ret = -1;
    switch (buffer) {
    case BOTH:
        ret = ftdi_usb_purge_buffers(ftdi);
        break;
    case RX:
        ret = ftdi_usb_purge_rx_buffer(ftdi);
        break;
    case TX:
        ret = ftdi_usb_purge_tx_buffer(ftdi);
        break;
    default:
        break;
    }
    return ret;
}



int close_device(ftdi_device ftdi)
{
    int ret;
    ret = ftdi_usb_close(ftdi);
    if (ret < 0)
    {
        fprintf(stderr, "Error closing device \n");
        return RETURN_ERROR;
    }
    ftdi_free(ftdi);
    return ret;
}

int read_data_from_device(ftdi_device ftdi,unsigned char* buffer, int size)
{
    int ret;
    ret = ftdi_read_data(ftdi, buffer, size);
//    if (buffer[0] == 0x55)
//   printf("Read %d bytes\n", ret);
//    for (int i= 0; i< ret; i++)
//        printf("   %x   ", buffer[i] & 0xff);
    return ret;
}

int write_data_to_device(ftdi_device ftdi,const unsigned char* buffer, const int size)
{
    int ret;
    ret = ftdi_write_data(ftdi, buffer, size);
    return ret;
}
