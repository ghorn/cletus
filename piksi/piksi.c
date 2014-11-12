#include "piksi.h"

#include <errno.h>
#include <termios.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <sys/ioctl.h>
#include "fifo.h"



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
    int fd;
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



int set_interface_attribs(int fd, int speed, int parity);
void set_blocking (int fd, int should_block);
u32 read_data(u8 *buff, u32 n, void *context __attribute__((unused)));

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
void init_message_processing(int buffer_size)
{
    /* SBP parser state must be initialized before sbp_process is called. */
    sbp_state_init(&piksi.state);
    piksi.buffer = malloc(buffer_size);
    fifo_init(&piksi.fifo, buffer_size);


}

int register_position_llh_callback(void* callback)
{
    return sbp_register_callback(&piksi.state, SBP_POS_LLH, callback,
                                 NULL, &piksi.message_nodes.pos_llh_node);
}


int register_velocity_ned_callback(void* callback)
{
    return sbp_register_callback(&piksi.state, SBP_VEL_NED, callback,
                                 NULL, &piksi.message_nodes.vel_ned_node);
}


int register_baseline_ned_callback(void* callback)
{
    return sbp_register_callback(&piksi.state, SBP_BASELINE_NED, callback,
                                 NULL, &piksi.message_nodes.baseline_ned_node);
}

int register_time_callback(void* callback)
{
    return sbp_register_callback(&piksi.state, SBP_GPS_TIME, callback,
                                 NULL, &piksi.message_nodes.gps_time_node);
}


int register_heartbeat_callback(void* callback)
{
    return sbp_register_callback(&piksi.state, SBP_HEARTBEAT, callback,
                                 NULL, &piksi.message_nodes.heartbeat_node);

}
int register_dops_callback(void* callback)
{
    return sbp_register_callback(&piksi.state, SBP_DOPS, callback,
                                 NULL, &piksi.message_nodes.dops_node);
}





void close_serial_port(void)
{
    close(piksi.fd);
}

int set_interface_attribs(int fd, int speed, int parity)
{
    struct termios tty;
    memset (&tty, 0, sizeof tty);
    if (tcgetattr (fd, &tty) != 0)
    {
        printf("error %d from tcgetattr", errno);
        return -1;
    }

    cfsetospeed (&tty, speed);
    cfsetispeed (&tty, speed);

    tty.c_cflag = (tty.c_cflag & ~CSIZE) | CS8;     // 8-bit chars
    // disable IGNBRK for mismatched speed tests; otherwise receive break
    // as \000 chars
    tty.c_iflag &= ~IGNBRK;         // disable break processing
    tty.c_lflag = 0;                // no signaling chars, no echo,
    // no canonical processing
    tty.c_oflag = 0;                // no remapping, no delays
    tty.c_cc[VMIN]  = 0;            // read doesn't block
    tty.c_cc[VTIME] = 5;            // 0.5 seconds read timeout

    tty.c_iflag &= ~(IXON | IXOFF | IXANY); // shut off xon/xoff ctrl

    tty.c_cflag |= (CLOCAL | CREAD);// ignore modem controls,
    // enable reading
    tty.c_cflag &= ~(PARENB | PARODD);      // shut off parity
    tty.c_cflag |= parity;
    tty.c_cflag &= ~CSTOPB;
    tty.c_cflag &= ~CRTSCTS;

    if (tcsetattr (fd, TCSANOW, &tty) != 0)
    {
        printf("error %d from tcsetattr", errno);
        return -1;
    }
    return 0;
}

void set_blocking (int fd, int should_block)
{
    struct termios tty;
    memset (&tty, 0, sizeof tty);
    if (tcgetattr (fd, &tty) != 0)
    {
        printf("error %d from tggetattr", errno);
        return;
    }

    tty.c_cc[VMIN]  = should_block ? 1 : 0;
    tty.c_cc[VTIME] = 5;            // 0.5 seconds read timeout

    if (tcsetattr (fd, TCSANOW, &tty) != 0)
        printf("error %d setting term attributes", errno);
}


u32 read_data(u8 *buff, u32 n, void *context __attribute__((unused))){
    //  printf("reading fifo thingy, length %d\n", n);
    return fifo_read(&piksi.fifo, buff, n);
}


int open_serial_port(const char* const device, int speed, int parity, int blocking)
{
    piksi.fd = open(device, O_RDWR | O_NOCTTY | O_SYNC | O_NONBLOCK);
    if (piksi.fd < 0)
    {
        printf("error %d opening %s: %s\n", errno, device, strerror (errno));
        return -1;
    }
    printf("opened %s successfully, setting up usb read\n", device);

    int ret = set_interface_attribs(piksi.fd, speed, parity);
    set_blocking(piksi.fd,blocking);
    return ret;

}



int process_messages(void)
{
    int bytes_in_file = 0;
    ioctl(piksi.fd, FIONREAD, bytes_in_file);
    if (bytes_in_file > 0)
    {
        int ret = read(piksi.fd, piksi.buffer, bytes_in_file);
        fifo_write(&piksi.fifo, piksi.buffer, bytes_in_file);
        do{
            ret = sbp_process(&piksi.state, &read_data);
        } while (piksi.fifo.tail != piksi.fifo.head);
        return ret;
    }
    return 0;
}

