#include <errno.h>
#include <termios.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>

#include <libswiftnav/sbp.h>
#include <libswiftnav/sbp_messages.h>
#include <libswiftnav/sbp_utils.h>

/*
 * State of the SBP message parser.
 * Must be statically allocated.
 */
sbp_state_t sbp_state;

/* Structs that messages from Piksi will feed. */
sbp_pos_llh_t      pos_llh;
sbp_baseline_ned_t baseline_ned;
sbp_vel_ned_t      vel_ned;
sbp_dops_t         dops;
sbp_gps_time_t     gps_time;
sbp_heartbeat_t    heartbeat;

/*
 * SBP callback nodes must be statically allocated. Each message ID / callback
 * pair must have a unique sbp_msg_callbacks_node_t associated with it.
 */
sbp_msg_callbacks_node_t pos_llh_node;
sbp_msg_callbacks_node_t baseline_ned_node;
sbp_msg_callbacks_node_t vel_ned_node;
sbp_msg_callbacks_node_t dops_node;
sbp_msg_callbacks_node_t gps_time_node;
sbp_msg_callbacks_node_t heartbeat_node;

/*
 * Callback functions to interpret SBP messages.
 * Every message ID has a callback associated with it to
 * receive and interpret the message payload.
 */
void sbp_pos_llh_callback(u16 sender_id __attribute__((unused)), u8 len __attribute__((unused)), u8 msg[], void *context __attribute__((unused)))
{
  pos_llh = *(sbp_pos_llh_t *)msg;
  printf("pos_llh\n");
}
void sbp_heartbeat_callback(u16 sender_id __attribute__((unused)), u8 len __attribute__((unused)), u8 msg[], void *context __attribute__((unused)))
{
  heartbeat = *(sbp_heartbeat_t *)msg;
  printf("heartbeat: %d\n", heartbeat.flags);
}
void sbp_baseline_ned_callback(u16 sender_id __attribute__((unused)), u8 len __attribute__((unused)), u8 msg[], void *context __attribute__((unused)))
{
  baseline_ned = *(sbp_baseline_ned_t *)msg;
  printf("baseline_ned (%.4f, %.4f, %.4f)\n", 
         1e-3*((double)baseline_ned.n),
         1e-3*((double)baseline_ned.e),
         1e-3*((double)baseline_ned.d));
}
void sbp_vel_ned_callback(u16 sender_id __attribute__((unused)), u8 len __attribute__((unused)), u8 msg[], void *context __attribute__((unused)))
{
  vel_ned = *(sbp_vel_ned_t *)msg;
  printf("                                              vel_ned (%.4f, %.4f, %.4f)\n", 
         1e-3*((double)baseline_ned.n),
         1e-3*((double)baseline_ned.e),
         1e-3*((double)baseline_ned.d));
  printf("vel_ned: (%d, %d, %d)\n", vel_ned.n, vel_ned.e, vel_ned.d);
}
void sbp_dops_callback(u16 sender_id __attribute__((unused)), u8 len __attribute__((unused)), u8 msg[], void *context __attribute__((unused)))
{
  dops = *(sbp_dops_t *)msg;
  printf("dops\n");
}
void sbp_gps_time_callback(u16 sender_id __attribute__((unused)), u8 len __attribute__((unused)), u8 msg[], void *context __attribute__((unused)))
{
  gps_time = *(sbp_gps_time_t *)msg;
  printf("time (%d, %d, %d)\n", gps_time.wn, gps_time.tow, gps_time.ns);
}

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
void sbp_setup(void)
{
  /* SBP parser state must be initialized before sbp_process is called. */
  sbp_state_init(&sbp_state);
  int ret = 0;

  /* Register a node and callback, and associate them with a specific message ID. */
  ret = sbp_register_callback(&sbp_state, SBP_HEARTBEAT, &sbp_heartbeat_callback,
                        NULL, &heartbeat_node);
  if (0 != ret) {printf("sbp_register_callback error: %d\n", ret); exit(-1);}
  ret = sbp_register_callback(&sbp_state, SBP_GPS_TIME, &sbp_gps_time_callback,
                        NULL, &gps_time_node);
  if (0 != ret) {printf("sbp_register_callback error: %d\n", ret); exit(-1);}
  ret = sbp_register_callback(&sbp_state, SBP_POS_LLH, &sbp_pos_llh_callback,
                        NULL, &pos_llh_node);
  if (0 != ret) {printf("sbp_register_callback error: %d\n", ret); exit(-1);}
  ret = sbp_register_callback(&sbp_state, SBP_BASELINE_NED, &sbp_baseline_ned_callback,
                        NULL, &baseline_ned_node);
  if (0 != ret) {printf("sbp_register_callback error: %d\n", ret); exit(-1);}
  ret = sbp_register_callback(&sbp_state, SBP_VEL_NED, &sbp_vel_ned_callback,
                        NULL, &vel_ned_node);
  if (0 != ret) {printf("sbp_register_callback error: %d\n", ret); exit(-1);}
  ret = sbp_register_callback(&sbp_state, SBP_DOPS, &sbp_dops_callback,
                        NULL, &dops_node);
  if (0 != ret) {printf("sbp_register_callback error: %d\n", ret); exit(-1);}
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

void
set_blocking (int fd, int should_block)
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

int fd = NULL;

u32 fifo_read(u8 *buff, u32 n, void *context __attribute__((unused))){
//  printf("reading fifo thingy, length %d\n", n);
  return read(fd, buff, n);
}

int main(){
  const char * const portname = "/dev/ttyUSB0";
  fd = open(portname, O_RDWR | O_NOCTTY | O_SYNC);
  if (fd < 0)
  {
    printf("error %d opening %s: %s\n", errno, portname, strerror (errno));
    return -1;
  }
  printf("opened %s successfully, setting up usb read\n", portname);


//  set_interface_attribs (fd, B115200, 0);  // set speed to 115,200 bps, 8n1 (no parity)
  set_interface_attribs (fd, B1000000, 0);  // set speed to 1,000,000 bps, 8n1 (no parity)
  set_blocking (fd, 1);                // set blocking
//  usleep ((7 + 25) * 100);             // sleep enough to transmit the 7 plus

  
  printf("setting up SBP\n");
  sbp_setup();
//  sbp_state_set_io_context(&sbp_state, &fd);

  printf("starting read loop\n");
  for (;;){
    s8 ret = sbp_process(&sbp_state, &fifo_read);
    if (ret < 0) {
      printf("sbp_process error: %d\n", (int)ret);
      //return ret;
    }
  }

  return 0;
}
