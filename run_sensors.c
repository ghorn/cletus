/* Copyright 2014 Matt Peddie <peddie@alum.mit.edu>
 *
 * This file is hereby placed in the public domain, or, if your legal
 * system does not recognize such a concept, you may consider it
 * licensed under BSD 3.0.  Use it for good.
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <signal.h>
#include <zmq.h>

#include "./zmq.h"
#include "./comms.h"
#include "./structures.h"
#include "./log.h"
#include "./sensors.h"
#include "./misc.h"

#ifdef ALL
#define IMU
#define RC
#define AHRS
#define AIRSPEED
#define GPS
#endif


const double gyro_scale_unit_coef = 0.0139882;
const double acc_scale_unit_coef = 0.0009766;
const double mag_scale_unit_coef = 0.0004883;
const double ahrs_unit_coef = 0.0000305;



/* ZMQ resources */
static void *zctx = NULL;
static void *zsock_imu = NULL;
static void *zsock_gps = NULL;
static void *zsock_rc = NULL;
static void *zsock_ahrs = NULL;
static void *zsock_airspeed = NULL;
static void *zsock_log = NULL;

/* Error tracking. */
int txfails = 0, rxfails = 0;

static void __attribute__((noreturn)) die(int code) {
  zdestroy(zsock_log, NULL);
  zdestroy(zsock_imu, zctx);
  zdestroy(zsock_gps, zctx);
  zdestroy(zsock_rc, zctx);
  zdestroy(zsock_ahrs, zctx);
  zdestroy(zsock_airspeed, zctx);


  printf("%d TX fails; %d RX fails.\n", txfails, rxfails);
  printf("Moriturus te saluto!\n");
  exit(code);
}

/* Our signal to GTFO */
static int bail = 0;

static void sigdie(int signum) {
  bail = signum;
}

int main(int argc __attribute__((unused)),
         char **argv __attribute__((unused))) {
  int errRet;
  //Communication setup
  uint8_t input_buffer[INPUT_BUFFER_SIZE];
  //init the data decode pointers
  init_decoding();
  errRet = serial_port_setup();
  if(errRet!=UART_ERR_NONE){
      err("couldn't initialize serial port");
      die(0);
    }




  setbuf(stdin, NULL);
  /* Confignals. */
  if (signal(SIGINT, &sigdie) == SIG_IGN)
    signal(SIGINT, SIG_IGN);
  if (signal(SIGTERM, &sigdie) == SIG_IGN)
    signal(SIGTERM, SIG_IGN);
  if (signal(SIGHUP, &sigdie) == SIG_IGN)
    signal(SIGHUP, SIG_IGN);
  if (signal(SIGABRT, &sigdie) == SIG_IGN)
    signal(SIGABRT, SIG_IGN);

  /* ZMQ setup first. */

  /* Set a low high-water mark (a short queue length, measured in
   * messages) so that a sending PUSH will block if the receiving PULL
   * isn't reading.  In the case of PUB/SUB, we still want a short
   * queue; it will prevent outdated messages from building up.  We
   * also set a small-ish buffer size so that the PUSH/PULL socket
   * pair will block or a PUB/SUB socket pair won't accumulate too
   * many outdated messages. */
  zsock_imu = setup_zmq_sender(IMU_CHAN, &zctx, ZMQ_PUSH, 2, 500);
  if (NULL == zsock_imu)
    return 1;;
  zsock_gps = setup_zmq_sender(GPS_CHAN, &zctx, ZMQ_PUSH, 2, 500);
  if (NULL == zsock_gps)
    return 1;;
  zsock_rc = setup_zmq_sender(RC_CHAN, &zctx, ZMQ_PUSH, 2, 500);
  if (NULL == zsock_rc)
    return 1;;
  zsock_ahrs = setup_zmq_sender(SENSORS_CHAN, &zctx, ZMQ_PUSH, 2, 500);
  if (NULL == zsock_ahrs)
    return 1;;
  zsock_airspeed = setup_zmq_sender(AIRSPEED_CHAN, &zctx, ZMQ_PUSH, 2, 500);
  if (NULL == zsock_ahrs)
    return 1;;
  /* Use big buffers here.  We're just publishing the data for
   * logging, so we don't mind saving some data until the logger can
   * receive it. */
  zsock_log = setup_zmq_sender(LOG_CHAN, &zctx, ZMQ_PUB, 1000, 100000);
  if (NULL == zsock_log)
    die(1);


  /* Sensor data storage. */
  sensors_t outgoing;

  zmq_pollitem_t poll_gps = {
    .socket=zsock_gps,
    .fd=-1,
    .events=0,
    .revents=0
  };


  zmq_pollitem_t poll_imu = {
    .socket=zsock_imu,
    .fd=-1,
    .events=0,
    .revents=0
  };

  zmq_pollitem_t poll_ahrs = {
    .socket=zsock_ahrs,
    .fd=-1,
    .events=0,
    .revents=0
  };


  zmq_pollitem_t poll_rc = {
    .socket=zsock_rc,
    .fd=-1,
    .events=0,
    .revents=0
  };

  zmq_pollitem_t poll_airspeed = {
    .socket=zsock_airspeed,
    .fd=-1,
    .events=0,
    .revents=0
  };

  zmq_pollitem_t poll_log = {
    .socket = zsock_log,
    .fd = -1,
    .events = 0,
    .revents = 0
  };

  zmq_pollitem_t polls[] = {
    poll_log,
    poll_gps,
    poll_imu,
    poll_ahrs,
    poll_rc,
    poll_airspeed
  };


  const int npolls = sizeof(polls) / sizeof(polls[0]);
  /* const int noutputs = npolls - ninputs; */

  /* Here's the main loop -- we only do stuff when input or output
   * happens.  The timeout can be put to good use, or you can also use
   * timerfd_create() to create a file descriptor with a timer under
   * the hood and dispatch on that.
   *
   * I apologize for the length of this loop.  For production code,
   * you'd want to pull most of the actual handling out into functions
   * and simply loop over your polls; I've left it all inline here
   * mostly out of laziness. */
  for (;;) {

      switch (get_lisa_data(&outgoing, input_buffer))
        {
#ifdef GPS
        case GPS_INT:
#if DEBUG

          printf("Received GPS Position data (X:%i ; Y:%i ; Z:%i\n",
                 outgoing.gps.pos_data.x,outgoing.gps.pos_data.y,outgoing.gps.pos_data.z);
#endif
          poll_gps.events =  ZMQ_POLLOUT;
          poll_log.events = ZMQ_POLLOUT;
          break;
#endif
#ifdef IMU
        case IMU_ACC_SCALED:
          xyz_convert_to_double(&(outgoing.imu.imu_accel.data), &(outgoing.imu.imu_accel_scaled.data),acc_scale_unit_coef );
#if DEBUG

          printf("Received Acceleration data (X:%f ; Y:%f; Z:%f\n",
                 outgoing.imu.imu_accel_scaled.data.x,outgoing.imu.imu_accel_scaled.data.y,outgoing.imu.imu_accel_scaled.data.z);
#endif
          poll_imu.events =  ZMQ_POLLOUT;
          poll_log.events = ZMQ_POLLOUT;
          break;
        case IMU_GYRO_SCALED:
          xyz_convert_to_double(&(outgoing.imu.imu_gyro.data), &(outgoing.imu.imu_gyro_scaled.data),gyro_scale_unit_coef );
#if DEBUG
          printf("Received Gyro data (X:%f ; Y:%f ; Z:%f\n",
                 outgoing.imu.imu_gyro_scaled.data.x,outgoing.imu.imu_gyro_scaled.data.y,outgoing.imu.imu_gyro_scaled.data.z);
#endif
          poll_imu.events =  ZMQ_POLLOUT;
          poll_log.events = ZMQ_POLLOUT;
          break;
        case IMU_MAG_SCALED:
          xyz_convert_to_double(&(outgoing.imu.imu_mag.data), &(outgoing.imu.imu_mag_scaled.data),mag_scale_unit_coef );
#if DEBUG
          printf("Received Mag data (X:%f ; Y:%f ; Z:%f\n",
                 outgoing.imu.imu_mag_scaled.data.x,outgoing.imu.imu_mag_scaled.data.y,outgoing.imu.imu_mag_scaled.data.z);
#endif
          poll_imu.events =  ZMQ_POLLOUT;
          poll_log.events = ZMQ_POLLOUT;
          break;
#endif
#ifdef AIRSPEED
        case AIRSPEED_ETS:
#if DEBUG
          printf("Received Airspeed data (X:%f ; Y:%f ; Z:%f\n",
                 outgoing.imu.imu_gyro_scaled.data.x,outgoing.imu.imu_gyro_scaled.data.y,outgoing.imu.imu_gyro_scaled.data.z);
#endif
          poll_airspeed.events =  ZMQ_POLLOUT;
          poll_log.events = ZMQ_POLLOUT;
          break;
#endif
#ifdef AHRS
        case AHRS_QUAT_INT:
          quat_convert_to_double(&(outgoing.ahrs.body), &(outgoing.ahrs.body_converted),ahrs_unit_coef );
          quat_convert_to_double(&(outgoing.ahrs.imu), &(outgoing.ahrs.imu_converted),ahrs_unit_coef );
#if DEBUG
          printf("Received AHRS body data (I:%f X:%f ; Y:%f ; Z:%f\n",
                 outgoing.ahrs.body_converted.qi,outgoing.ahrs.body_converted.qx,outgoing.ahrs.body_converted.qy,outgoing.ahrs.body_converted.qz);
          printf("Received AHRS IMU data (I:%f X:%f ; Y:%f ; Z:%f\n",
                 outgoing.ahrs.imu_converted.qi,outgoing.ahrs.imu_converted.qx,outgoing.ahrs.imu_converted.qy,outgoing.ahrs.imu_converted.qz);
#endif
          poll_airspeed.events =  ZMQ_POLLOUT;
          poll_log.events = ZMQ_POLLOUT;
          break;
#endif
#ifdef RC
        case ROTORCRAFT_RADIO_CONTROL:
#if DEBUG
          printf("RC Data: Mode: %i Status: %i \n", outgoing.rc.mode,outgoing.rc.status);
#endif
#endif
        default:
          break;
        }


      if (bail) die(bail);
      /* Poll for activity; time out after 10 milliseconds. */
      const int polled = zmq_poll(polls, npolls, 10);
      if (polled < 0) {
          if (bail) die(bail);
          zerr("while polling");
          /* not sure what to do about it. */
          continue;
        } else if (polled == 0) {
          if (bail) die(bail);
          /* timeout! */
          continue;
        }

      if (bail) die(bail);
      usleep(5000); // 200 Hz


      if (bail) die(bail);
      if (poll_gps.revents & ZMQ_POLLOUT) {
          const void *bufs[] = {&outgoing.gps};
          const uint32_t lens[] = {sizeof(gps_t)};
          const int zs = zmq_sendm(zsock_gps, bufs, lens,
                                   sizeof(lens) / sizeof(lens[0]));
          if (zs < 0) {
              txfails++;
            } else {
              printf("GPS sent to controller!, size: %d\n", (int)sizeof(gps_t));
              poll_gps.events = 0;
            }
          poll_gps.revents = 0;
        }

      if (poll_imu.revents & ZMQ_POLLOUT) {
          const void *bufs[] = {&outgoing.imu};
          const uint32_t lens[] = {sizeof(imu_t)};
          const int zs = zmq_sendm(zsock_imu, bufs, lens,
                                   sizeof(lens) / sizeof(lens[0]));
          if (zs < 0) {
              txfails++;
            } else {
              printf("IMU sent to controller!, size: %d\n", (int)sizeof(imu_t));
              poll_imu.events = 0;
            }
          poll_imu.revents = 0;
        }

      if (poll_ahrs.revents & ZMQ_POLLOUT) {
          const void *bufs[] = {&outgoing.ahrs};
          const uint32_t lens[] = {sizeof(ahrs_t)};
          const int zs = zmq_sendm(zsock_imu, bufs, lens,
                                   sizeof(lens) / sizeof(lens[0]));
          if (zs < 0) {
              txfails++;
            } else {
              printf("AHRS sent to controller!, size: %d\n", (int)sizeof(ahrs_t));
              poll_ahrs.events = 0;
            }
          poll_ahrs.revents = 0;
        }

      if (bail) die(bail);
      if ((poll_log.revents > 3) & ZMQ_POLLOUT) {
          const uint8_t type = LOG_MESSAGE_SENSORS;
          const void *bufs[] = {&type, &outgoing};
          const uint32_t lens[] = {sizeof(type), sizeof(outgoing)};
          const int zs = zmq_sendm(zsock_log, bufs, lens,
                                   sizeof(lens) / sizeof(lens[0]));
          if (zs < 0) {
              txfails++;
            } else {
              printf("Sent to logger!\n");
              /* Clear the events flag so we won't try to send until we
               * have more data. */
              poll_log.events = 0;
            }
          poll_log.revents = 0;
        }
    }

  /* Shouldn't get here. */
  return 0;
}
