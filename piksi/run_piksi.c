#include <errno.h>
#include <termios.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <inttypes.h>
#include "./piksi.h"


/* Structs that messages from Piksi will feed. */
piksi_position_llh_t      pos_llh;
piksi_baseline_ned_t baseline_ned;
piksi_velocity_ned_t      vel_ned;
piksi_dops_t         dops;
piksi_time_t     gps_time;
piksi_heartbeat_t    heartbeat;


/*
 * Callback functions to interpret SBP messages.
 * Every message ID has a callback associated with it to
 * receive and interpret the message payload.
 */
void sbp_pos_llh_callback(u_int16_t sender_id __attribute__((unused)), u_int8_t len __attribute__((unused)), u8 msg[], void *context __attribute__((unused)))
{
    pos_llh = *(sbp_pos_llh_t *)msg;
    printf("pos_llh  %.3f, %d\n",
           1e-3*((double)pos_llh.tow), pos_llh.flags);
}
void sbp_heartbeat_callback(u_int16_t sender_id __attribute__((unused)), u_int8_t len __attribute__((unused)), u8 msg[], void *context __attribute__((unused)))
{
    heartbeat = *(sbp_heartbeat_t *)msg;
    printf("heartbeat: %d\n", heartbeat.flags);
}
void sbp_baseline_ned_callback(u_int16_t sender_id __attribute__((unused)), u_int8_t len __attribute__((unused)), u8 msg[], void *context __attribute__((unused)))
{
    static piksi_baseline_status_t status;
    baseline_ned = *(sbp_baseline_ned_t *)msg;
    printf("baseline_ned (%.3f, %.4f, %.4f, %.4f)\n",
           1e-3*((double)baseline_ned.tow),
           1e-3*((double)baseline_ned.n),
           1e-3*((double)baseline_ned.e),
           1e-3*((double)baseline_ned.d));
    memcpy(&status, &baseline_ned.flags, sizeof(piksi_baseline_status_t));
    printf("Status: %i \n", status.fix_status);
}
void sbp_vel_ned_callback(u_int16_t sender_id __attribute__((unused)), u_int8_t len __attribute__((unused)), u8 msg[], void *context __attribute__((unused)))
{
    vel_ned = *(sbp_vel_ned_t *)msg;
    printf("                                              vel_ned (%.4f, %.4f, %.4f)\n",
           1e-3*((double)baseline_ned.n),
           1e-3*((double)baseline_ned.e),
           1e-3*((double)baseline_ned.d));
    printf("vel_ned: (%d, %d, %d)\n", vel_ned.n, vel_ned.e, vel_ned.d);
}
void sbp_dops_callback(u_int16_t sender_id __attribute__((unused)), u_int8_t len __attribute__((unused)), u8 msg[], void *context __attribute__((unused)))
{
    dops = *(sbp_dops_t *)msg;
    printf("dops\n");
}
void sbp_gps_time_callback(u_int16_t sender_id __attribute__((unused)), u_int8_t len __attribute__((unused)), u8 msg[], void *context __attribute__((unused)))
{
    gps_time = *(sbp_gps_time_t *)msg;
    printf("time (%d, %d, %d)\n", gps_time.wn, gps_time.tow, gps_time.ns);
}


int main(){
    const char * const portname = "/dev/ttyUSB0";
    open_serial_port(portname, B1000000, 0, 1 ); // set speed to 1,000,000 bps, 8n1 (no parity) set blocking

    init_message_processing(512);
    int ret= 0;
    /* Register a node and callback, and associate them with a specific message ID. */
//    ret =register_heartbeat_callback(&sbp_heartbeat_callback);
//    if (0 != ret) {printf("sbp_register_callback error: %d\n", ret); exit(-1);}
//    ret = register_time_callback(&sbp_gps_time_callback);
    if (0 != ret) {printf("sbp_register_callback error: %d\n", ret); exit(-1);}
    ret = register_position_llh_callback(&sbp_pos_llh_callback);
    if (0 != ret) {printf("sbp_register_callback error: %d\n", ret); exit(-1);}
    ret = register_baseline_ned_callback(&sbp_baseline_ned_callback);
//    if (0 != ret) {printf("sbp_register_callback error: %d\n", ret); exit(-1);}
//    ret = register_velocity_ned_callback(&sbp_vel_ned_callback);
//    if (0 != ret) {printf("sbp_register_callback error: %d\n", ret); exit(-1);}
//    ret = register_dops_callback(&sbp_dops_callback);
//    if (0 != ret) {printf("sbp_register_callback error: %d\n", ret); exit(-1);}

    //  sbp_state_set_io_context(&sbp_state, &fd);

    printf("starting read loop\n");
    for (;;){
        ret = process_messages();
        if (ret < 0) {
            printf("sbp_process error: %d\n", (int)ret);
            //return ret;
        }
    }

    return 0;
}

