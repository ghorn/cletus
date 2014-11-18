#ifndef PIKSI_H
#define PIKSI_H

#include "./libswiftnav/include/libswiftnav/sbp.h"
#include "./libswiftnav/include/libswiftnav/sbp_messages.h"
#include "./libswiftnav/include/libswiftnav/sbp_utils.h"

typedef sbp_pos_llh_t piksi_position_llh_t;
typedef sbp_baseline_ned_t piksi_baseline_ned_t;
typedef sbp_baseline_ecef_t piksi_baseline_ecef_t;
typedef sbp_vel_ned_t piksi_velocity_ned_t;
typedef sbp_vel_ecef_t piksi_velocity_ecef_t;
typedef sbp_dops_t piksi_dops_t;
typedef sbp_gps_time_t piksi_time_t;
typedef sbp_heartbeat_t piksi_heartbeat_t;


typedef struct __attribute__((packed)){
    uint8_t reserved:5;
    uint8_t fix_status:3;
} piksi_baseline_status_t;


typedef struct __attribute__((packed)){
    uint8_t reserved:4;
    uint8_t height_status:1;
    uint8_t fix_status:3;
} piksi_position_status_t;

#define PIKSI_STATUS_FLOAT 0
#define PIKSI_STATUS_FIXED_RTK 1
#define PIKSI_STATUS_HEIGHT_ELLIPSOID 0
#define PIKSI_STATUS_HEIGHT_SEA_LEVEL 1






int register_position_llh_callback(void* callback);
int register_velocity_ned_callback(void* callback);
int register_baseline_ned_callback(void* callback);
int register_time_callback(void* callback);
int register_heartbeat_callback(void* callback);
int register_dops_callback(void* callback);
int open_serial_port(const char* const device, int speed, int parity, int blocking);
void close_serial_port(void);
int process_messages(void);
void init_message_processing(int buffer_size);





#endif // PIKSI_H
