#include "piksi.h"

#include <errno.h>
#include <termios.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include "../ftdi_device.h"
#include "fifo.h"

#define PIKSI_BAUDRATE 115200
#define PIKSI_DEVICE 1
#define PIKSI_BIT_TYPE 8
#define PIKSI_PARITY 0
#define PIKSI_LATENCY 2
#define PIKSI_READ_CHUNKSIZE 512
#define PIKSI_WRITE_CHUNKSIZE 512

/*
 * SBP callback nodes must be statically allocated. Each message ID / callback
 * pair must have a unique sbp_msg_callbacks_node_t associated with it.
 */
typedef struct {
    sbp_msg_callbacks_node_t pos_llh_node;
    sbp_msg_callbacks_node_t baseline_ned_node;
    sbp_msg_callbacks_node_t vel_ned_node;
    sbp_msg_callbacks_node_t dops_node;
    sbp_msg_callbacks_node_t gps_time_node;
    sbp_msg_callbacks_node_t heartbeat_node;
} piksi_nodes_t;

typedef struct {
    sbp_state_t state;
    piksi_nodes_t message_nodes;
    fifo_t fifo;
    ftdi_device ftdi;
    void* buffer;
} piksi_t;


/*
 * State of the SBP message parser.
 * Must be statically allocated.
 */
piksi_t piksi;


/* Structs that messages from Piksi will feed. */
sbp_pos_llh_t      pos_llh;
sbp_baseline_ned_t baseline_ned;
sbp_vel_ned_t      vel_ned;
sbp_dops_t         dops;
sbp_gps_time_t     gps_time;
sbp_heartbeat_t    heartbeat;




u32 piksi_read_data(u8 *buff, u32 n, void *context __attribute__((unused)));

/*
 * Set up SwiftNav Binary Protocol (SBP) nodes; the sbp_process function will
 * search through these to find the callback for a particular message ID.
 *
 * Example: sbp_pos_llh_callback is registered with sbp_state, and is associated
 * with both a unique sbp_msg_callbacks_node_t and the message ID SBP_POS_LLH.
 * When a valid SBP message with the ID SBP_POS_LLH comes through the UART, written
 * to the FIFO, and then parsed by sbp_process, sbp_pos_llh_callback is called
 * with the data carried by that message.
 */
void piksi_init_message_processing(int buffer_size)
{
    /* SBP parser state must be initialized before sbp_process is called. */
    sbp_state_init(&piksi.state);
    piksi.buffer = malloc(buffer_size);
    fifo_init(&piksi.fifo, buffer_size);


}

int piksi_register_position_llh_callback(void* callback)
{
    return sbp_register_callback(&piksi.state, SBP_POS_LLH, callback,
                                 NULL, &piksi.message_nodes.pos_llh_node);
}


int piksi_register_velocity_ned_callback(void* callback)
{
    return sbp_register_callback(&piksi.state, SBP_VEL_NED, callback,
                                 NULL, &piksi.message_nodes.vel_ned_node);
}


int piksi_register_baseline_ned_callback(void* callback)
{
    return sbp_register_callback(&piksi.state, SBP_BASELINE_NED, callback,
                                 NULL, &piksi.message_nodes.baseline_ned_node);
}

int piksi_register_time_callback(void* callback)
{
    return sbp_register_callback(&piksi.state, SBP_GPS_TIME, callback,
                                 NULL, &piksi.message_nodes.gps_time_node);
}


int piksi_register_heartbeat_callback(void* callback)
{
    return sbp_register_callback(&piksi.state, SBP_HEARTBEAT, callback,
                                 NULL, &piksi.message_nodes.heartbeat_node);

}
int piksi_register_dops_callback(void* callback)
{
    return sbp_register_callback(&piksi.state, SBP_DOPS, callback,
                                 NULL, &piksi.message_nodes.dops_node);
}





void piksi_close_connection(void)
{
    int ret = close_device(piksi.ftdi);
    if (ret < 0 )
    {
        printf ("Error closing PIKSI device\n");
    }

}


u32 piksi_read_data(u8 *buff, u32 n, void *context __attribute__((unused))){
    //printf("reading fifo thingy, length %d\n", n);
    return  read_data_from_device(piksi.ftdi, buff, n);

}


int piksi_open_connection(void)
{
    int ret = 0;
    open_device(&piksi.ftdi,PIKSI_DEVICE, PIKSI_BAUDRATE, PIKSI_BIT_TYPE, PIKSI_PARITY);
    if (ret < 0 || piksi.ftdi == NULL)
    {
        printf("Error opening Piksi device\n");
        return ret;
    }
    ret = set_chunksize(piksi.ftdi, PIKSI_READ_CHUNKSIZE, PIKSI_WRITE_CHUNKSIZE);
    if (ret < 0)
    {
        printf("Error setting Piksi chunksize\n");
        return ret;
    }
    ret = set_latency_timer(piksi.ftdi, PIKSI_LATENCY);
    if (ret < 0)
    {
        printf("Error setting Piksi latency");
        return ret;
    }
    flush_device(piksi.ftdi, BOTH);
    return ret;





}

int piksi_flush_buffers(void)
{
    return flush_device(piksi.ftdi, BOTH);
}


int piksi_read_message(void)
{

    int ret;
    do {
    ret = sbp_process(&piksi.state, &piksi_read_data);
    } while (ret != 0);
    return ret;
}

