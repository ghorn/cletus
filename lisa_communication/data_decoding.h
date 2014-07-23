#ifndef DATA_DECODING_H_ 
#define DATA_DECODING_H_

/**************************************************************************************
This library is used to decode, encode and modify packets that are sent between bbones
* and the server over UDP
#
****************************************************************************************/

/**************************************************************************************
* LAYOUT OF INCOMING PACKAGES 
* startbyte (0x99) - length - sender_id, message_id, message ... , checksumA, checksumB
****************************************************************************************/

#ifdef __cplusplus
extern "C"
{
#endif

#include <stdint.h>
#include <sys/time.h>
#include <time.h>
#include "time_highwind.h"
#include "../structures.h"

/********************************
 * GLOBALS
 * ******************************/
//message ids
enum Message_id{
  SYSMON = 33,
  UART_ERRORS = 208,
  ACTUATORS = 105,
  SVINFO=25,
  AIRSPEED_ETS = 57,
  GPS_INT=155,
  IMU_GYRO_SCALED=131,
  IMU_ACC_SCALED=132,
  IMU_MAG_SCALED=133,
  BARO_RAW = 221,
  IMU_GYRO_RAW = 203,
  IMU_ACCEL_RAW = 204,
  IMU_MAG_RAW = 205,
  IMU_GYRO = 200,
  IMU_ACCEL = 202,
  IMU_MAG = 201,
  SERVO_COMMANDS = 72,
  AHRS_QUAT_INT = 157,
  ROTORCRAFT_RADIO_CONTROL = 160
};

//sender ids
enum Sender_id{
  SERVER = 1,
  BONE_PLANE=2,
  BETTY=165,
  BABY_BETTY=166,
  BONE_WIND=3,
  BONE_ARM=4
};

//import indexes of incoming data array
enum stream_index{
  STARTBYTE_INDEX=0,
  LENGTH_INDEX,
  SENDER_ID_INDEX,
  MESSAGE_ID_INDEX,
  MESSAGE_START_INDEX
};

// Decoding error codes 
enum dec_errCode {
  DEC_ERR_NONE = 0,
  DEC_ERR_START_BYTE,
  DEC_ERR_CHECKSUM,
  DEC_ERR_UNKNOWN_BONE_PACKAGE,
  DEC_ERR_UNKNOWN_LISA_PACKAGE,
  DEC_ERR_UNKNOWN_WIND_PACKAGE,
  DEC_ERR_UNKNOWN_SENDER,DEC_ERR_LENGTH,
  DEC_ERR_UNDEFINED
};
typedef enum dec_errCode DEC_errCode;

//library numbers to know which library generated error on beaglebone 
enum Library {
  UDP_L,
  UART_L,
  DECODE_L,
  LOG_L,
  CIRCULAR_BUFFER_L,
  SPI_L
};
typedef enum Library library; 

//pragma to set internal memory alignment to 1 byte so we can fill the structs binary
#pragma pack(push)  /* push current alignment to stack */
#pragma pack(1)     /* set alignment to 1 byte boundary */

typedef struct {
  uint64_t tv_sec;
  uint64_t tv_usec;
} Timeval16; //redefine a new 16byte timeval for beaglebone because beaglebone has 8 byte timeval

//typedef Timeval32 timeval;

typedef union{
  uint8_t raw[16];
  Timeval16 tv;
} TimestampBeagle;


/************************************
 * OUTGOING MESSAGES
 * **********************************/

typedef union{ //message id 72
  uint8_t raw[14];
  struct Output_message {
    int16_t servo_1;
    int16_t servo_2;
    int16_t servo_3;
    int16_t servo_4;
    int16_t servo_5;
    int16_t servo_6;
    int16_t servo_7;
  } message;
} Output;

/************************************
 * INCOMING MESSAGES
 * **********************************/


//these structure are created from messages.xml in the paparazzi code + timestamp
typedef struct { // id = 33
  uint16_t periodic_time;
  uint16_t periodic_cycle;
  uint16_t periodic_cycle_min;
  uint16_t periodic_cycle_max;
  uint16_t event_number;
  uint8_t cpu_load; //in %
  struct timeval tv;
  int8_t new_data;
} Sys_mon;

typedef struct { // id = 208
  uint16_t overrun_cnt;
  uint16_t noise_err_cnt;
  uint16_t framing_err_cnt;
  uint8_t bus_number;
  struct timeval tv;
  int8_t new_data;
} UART_errors;

typedef struct { // id = 105
  uint8_t arr_length;
  int16_t values[7]; //the ACTUATORS message contains the final commands to the servos (or any actuator) regardless of which mode you are in (e.g. if it's comming from RC or NAV)
  struct timeval tv;
  int8_t new_data;
} Actuators;

typedef struct { // id = 25
  uint8_t chn;
  uint8_t svid;
  uint8_t flags;
  uint8_t qi;
  uint8_t cno;
  int8_t elev;
  uint16_t azim;
  struct timeval tv;
  int8_t new_data;
} Svinfo;

typedef struct { // id = 57
  uint16_t adc;
  uint16_t offset;
  float scaled;
  struct timeval tv;
  int8_t new_data;
} Airspeed_ets;

typedef struct { // id = 155
  int32_t ecef_x;
  int32_t ecef_y;
  int32_t ecef_z;
  int32_t lat;
  int32_t lon;
  int32_t alt;
  int32_t hmsl;
  int32_t ecef_xd;
  int32_t ecef_yd;
  int32_t ecef_zd;
  int32_t pacc;
  int32_t sacc;
  uint32_t tow;
  uint16_t pdop;
  uint8_t numsv;
  uint8_t fix;
  struct timeval tv;
  int8_t new_data;
} Gps_int;


typedef struct { // id = 202
  float ax;
  float ay;
  float az;
  struct timeval tv;
  int8_t new_data;
} Imu_accel;

typedef struct { // id = 200
  float gp;
  float gq;
  float gr;
  struct timeval tv;
  int8_t new_data;
} Imu_gyro;


typedef struct { // id = 201
  float mx;
  float my;
  float mz;
  struct timeval tv;
  int8_t new_data;
} Imu_mag;

typedef struct { // id = 221
  int32_t abs;
  int32_t diff;
  struct timeval tv;
  int8_t new_data;
} Baro_raw;





typedef struct { // id = 203
  int32_t gp;
  int32_t gq;
  int32_t gr;
  struct timeval tv;
  int8_t new_data;
} Imu_gyro_raw;

typedef struct { // id = 204
  int32_t ax;
  int32_t ay;
  int32_t az;
  struct timeval tv;
  int8_t new_data;
} Imu_accel_raw;


typedef struct { // id = 205
  int32_t mx;
  int32_t my;
  int32_t mz;
  struct timeval tv;
  int8_t new_data;
} Imu_mag_raw;


typedef struct { // sender id = 165
  Svinfo svinfo;
  Airspeed_ets airspeed_ets;
  Gps_int gps_int;
  Baro_raw baro_raw;
  Imu_gyro_raw imu_gyro_raw;
  Imu_accel_raw imu_accel_raw;
  Imu_mag_raw imu_mag_raw;
  Sys_mon sys_mon;
  UART_errors uart_errors;
  Actuators actuators;
  Imu_gyro imu_gyro;
  Imu_accel imu_accel;
  Imu_mag imu_mag;
} Lisa_plane;

typedef struct
{
  Lisa_plane lisa_plane;
  sensors_t zmq_sensors;
} Data;



#pragma pack(pop)   /* restore original alignment from stack */

/********************************
 * PROTOTYPES PUBLIC
 * ******************************/
extern void init_decoding(void);/*initalize read/write pointers*/
extern DEC_errCode data_decode(unsigned char stream[]);/*decodes data stream to the right structure*/
extern void switch_read_write(void);
extern DEC_errCode data_encode(unsigned char message[],long unsigned int message_length,unsigned char encoded_data[],int sender_id,int message_id);
//extern Data* get_read_pointer(); /*to get read access to data structure*/
extern void calculate_checksum(unsigned char buffer[],uint8_t *checksum_1,uint8_t *checksum2);
extern int add_timestamp(unsigned char buffer[]);/*add timestamp to existing package, updates the checksum and the length byte*/
extern int strip_timestamp(unsigned char buffer[]);/*removes timestamp, update checksums and length byte*/
DEC_errCode NMEA_asci_encode(const unsigned char buffer[], unsigned char encoded_data[]);/*Encodes NMEA packages coming from windsensor*/
void DEC_err_handler(DEC_errCode err,void (*write_error_ptr)(char *,char *,int));  
void get_new_sensor_struct(sensors_t * const data_struct);
uint8_t set_actuators(actuators_t* const message, unsigned char encoded_data[]);


#ifdef __cplusplus
}
#endif

#endif /*DATA_DECODING_H_*/
