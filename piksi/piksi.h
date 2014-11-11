#ifndef PIKSI_H
#define PIKSI_H

#include "./libswiftnav/include/libswiftnav/sbp.h"
#include "./libswiftnav/include/libswiftnav/sbp_messages.h"
#include "./libswiftnav/include/libswiftnav/sbp_utils.h"

typedef sbp_pos_llh_t piksi_position_t;
typedef sbp_baseline_ned_t piksi_baseline_t;
typedef sbp_vel_ned_t piksi_velocity_t;
typedef sbp_dops_t piksi_dops_t;
typedef sbp_gps_time_t piksi_time_t;
typedef sbp_heartbeat_t piksi_heartbeat_t;



int register_position_callback(void* callback);
int register_velocity_callback(void* callback);
int register_baseline_callback(void* callback);
int register_time_callback(void* callback);
int register_heartbeat_callback(void* callback);
int register_dops_callback(void* callback);
int open_serial_port(const char* const device, int speed, int parity, int blocking);
int process_messages(void);
void init_message_processing(void);





#endif // PIKSI_H
