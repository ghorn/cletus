#include <stdlib.h>
#include "data_decoding.h"
#include <string.h>

#ifndef DEBUG 
#define DEBUG 0
#endif

#include <stdio.h>

/********************************
 * PROTOTYPES PRIVATE
 * ******************************/
static DEC_errCode data_to_struct(unsigned char sender,unsigned char stream[], int length); 
void data_write(unsigned char stream[],void *destination, int length);

/********************************
 * GLOBALS
 * ******************************/

//one writes to ping and another can read data from pong and upside down
static Data ping;
static Data pong;

//pointers to ping and pong
static Data* read_data;
static Data* write_data;


/********************************
 * FUNCTIONS
 * ******************************/

void init_decoding(){
#if DEBUG > 1
    printf("Entering init_decoding\n");
#endif

    read_data = &ping;
    write_data = &pong;
}

void switch_read_write()
{
#if DEBUG  > 1
    printf("Entering switch_read_write\n");
#endif

    Data* temp = read_data;

    read_data = write_data;
    write_data = temp;
}

DEC_errCode data_decode(unsigned char stream[])
{
#if DEBUG  > 1
    printf("Entering data_decode\n");
#endif

    uint8_t checksum_1 = 0;
    uint8_t checksum_2 = 0;
    uint8_t length = stream[LENGTH_INDEX];
    uint8_t sender = stream[SENDER_ID_INDEX];

    //check first bit is 0x99
    if(stream[STARTBYTE_INDEX] != 0x99)
    {
        //unknown package !!!
        return DEC_ERR_START_BYTE;
    }

    calculate_checksum(stream,&checksum_1,&checksum_2);

    //checksum1 is voorlaatste byte, checksum2 is last byte
    if(checksum_1 != stream[length-2] || checksum_2 != stream[length-1])
    {
        return DEC_ERR_CHECKSUM;
    }

    return data_to_struct(sender, stream, length);
}

static DEC_errCode data_to_struct(unsigned char sender,unsigned char stream[], int length) // start = 0
{
#if DEBUG  > 1
    printf("Entering data_to_struct\n");
#endif

    switch(sender)
    {
    case BONE_PLANE: //sender_id of beaglebone
        switch(stream[MESSAGE_ID_INDEX])
        {
        case BEAGLE_ERROR:
            data_write(stream, (void *)&write_data->bone_plane.error, sizeof(Beagle_error)-1);
            write_data->bone_plane.error.new_data = 0;
            break;
        default: return DEC_ERR_UNKNOWN_BONE_PACKAGE; break;
        }
        break;
    case LISA: //sender_id of lisa
#if DEBUG  > 1
        printf("Received data packet from LISA of type");
#endif
        switch(stream[MESSAGE_ID_INDEX]) // the message id of the folowing message
        {
        case SVINFO:
#if DEBUG  > 1
            printf("SVINFO\n");
#endif
            data_write(stream, (void *)&write_data->lisa_plane.svinfo, sizeof(Svinfo)-1);
            break;
        case SYSMON:
#if DEBUG  > 1
            printf("SYSMON\n");
#endif
            data_write(stream, (void *)&write_data->lisa_plane.sys_mon, sizeof(Sys_mon)-1);
            break;
        case AIRSPEED_ETS:
#if DEBUG  > 1
            printf("AIRSPEED_ETS\n");
#endif
            data_write(stream, (void *)&write_data->lisa_plane.airspeed_ets, sizeof(Airspeed_ets)-1);
            break;
        case ACTUATORS:
#if DEBUG  > 1
            printf("ACTUATORS\n");
#endif
            data_write(stream, (void *)&write_data->lisa_plane.actuators, sizeof(Actuators)-1);
            break;
        case GPS_INT:
#if DEBUG  > 1
            printf("GPS_INT\n");
#endif
            data_write(stream, (void *)&write_data->lisa_plane.gps_int, sizeof(Gps_int)-1);
            break;
        case IMU_GYRO_RAW:
#if DEBUG  > 1
            printf("IMU_GYRO_RAW\n");
#endif
            data_write(stream, (void *)&write_data->lisa_plane.imu_gyro_raw, sizeof(Imu_gyro_raw)-1);
            break;

        case IMU_ACCEL_RAW:
#if DEBUG  > 1
            printf("IMU_ACCEL_RAW\n");
#endif
            data_write(stream, (void *)&write_data->lisa_plane.imu_accel_raw, sizeof(Imu_accel_raw)-1);
            break;

        case IMU_MAG_RAW:
#if DEBUG  > 1
            printf("IMU_MAG_RAW\n");
#endif
            data_write(stream, (void *)&write_data->lisa_plane.imu_mag_raw, sizeof(Imu_mag_raw)-1);
            break;

        case UART_ERRORS:
#if DEBUG  > 1
            printf("UART_ERRORS\n");
#endif
            data_write(stream, (void *)&write_data->lisa_plane.uart_errors, sizeof(UART_errors)-1);
            break;

        case BARO_RAW:
#if DEBUG  > 1
            printf("BARO_RAW\n");
#endif
            data_write(stream, (void *)&write_data->lisa_plane.baro_raw, sizeof(Baro_raw)-1);
            break;
        default:
#if DEBUG  > 1
            printf("UNKNOWN\n");
#endif
            return DEC_ERR_UNKNOWN_LISA_PACKAGE;break;
        }
        break;
    case BONE_WIND:
        switch(stream[MESSAGE_ID_INDEX]){
        case NMEA_IIMWV_ID:
            data_write(stream, (void *)&write_data->bone_wind.nmea_iimmwv, sizeof(NMEA_IIMWV)-1);
            break;
        case NMEA_WIXDR_ID:
            data_write(stream, (void *)&write_data->bone_wind.nmea_wixdr, sizeof(NMEA_WIXDR)-1);
            break;
        default: return DEC_ERR_UNKNOWN_WIND_PACKAGE; break;
        }
        break;
    case BONE_ARM:
        switch(stream[MESSAGE_ID_INDEX]){
        case LINE_ANGLE_ID:
            data_write(stream, (void *)&write_data->bone_arm.line_angle, sizeof(LINE_ANGLE)-1);
            break;
        default: return DEC_ERR_UNKNOWN_WIND_PACKAGE; break;
        }
        break;

    default: return DEC_ERR_UNKNOWN_SENDER; break;
    }
    return DEC_ERR_NONE;
}

void data_write(unsigned char stream[],void *destination, int length)
{
#if DEBUG  > 1
    printf("Entering data_write\n");
#endif

    memcpy(destination,&(stream[MESSAGE_START_INDEX]),length);
}


DEC_errCode NMEA_asci_encode(const unsigned char buffer[], unsigned char encoded_data[])
{
#if DEBUG  > 1
    printf("Entering NMEA_asci_encode\n");
#endif
    char str_IIMWV[6];
    strcpy(str_IIMWV,"IIMWV");
    char str_WIXDR[6];
    strcpy(str_WIXDR,"WIXDR");

    if(strncmp((const char *) &buffer[1],str_IIMWV,5)==0){
        NMEA_IIMWV wind;
        sscanf((const char *) &buffer[7],"%lf",&wind.wind_angle);
        sscanf((const char *) &buffer[13],"%c",&wind.relative);
        sscanf((const char *) &buffer[15],"%lf",&wind.wind_speed);
        sscanf((const char *) &buffer[22],"%c",&wind.wind_speed_unit);
        sscanf((const char *) &buffer[24],"%c",&wind.status);
        data_encode((unsigned char *)&wind,sizeof(NMEA_IIMWV)-16-1,encoded_data,BONE_WIND,NMEA_IIMWV_ID); //- timestamp and - new data flag

        /*printf("wind angle %lf\n",wind.wind_angle);
        printf("relative %c\n",wind.relative);
        printf("wind speed %lf\n",wind.wind_speed);
        printf("wind speed unit %c\n",wind.wind_speed_unit);
        printf("status %c\n",wind.status);
        printf("\n");*/

    }else if(strncmp((const char *) &buffer[1],str_WIXDR,5)==0){
        if(buffer[7]=='C'){
            NMEA_WIXDR temp;
            sscanf((const char *) &buffer[9],"%lf",&temp.temperature);
            sscanf((const char *) &buffer[15],"%c",&temp.unit);
            data_encode((unsigned char *)&temp,sizeof(NMEA_WIXDR)-16-1,encoded_data,BONE_WIND,NMEA_WIXDR_ID); //- timestamp and - new data flag

            /*printf("Temperature %lf\n",temp.temperature);
                printf("unit %c\n",temp.unit);
                printf("\n");*/
        }
    }else{
        return DEC_ERR_UNKNOWN_WIND_PACKAGE;
    }
    return DEC_ERR_NONE;
}

DEC_errCode data_encode(unsigned char message[],long unsigned int message_length,unsigned char encoded_data[],int sender_id,int message_id)
{
#if DEBUG  > 1
    printf("Entering data_encode\n");
#endif

    uint8_t checksum_1 = 0;
    uint8_t checksum_2 = 0;
    uint8_t length = message_length+6+16; //message length + 6 info bytes + 16 timestamp bytes
    timeval timestamp;
    TimestampBeagle timestampBeagle;

    encoded_data[STARTBYTE_INDEX] = 0x99;
    encoded_data[LENGTH_INDEX] = length;
    encoded_data[SENDER_ID_INDEX] = sender_id; // sender id of server
    encoded_data[MESSAGE_ID_INDEX] = message_id; // message id

    //add message
    memcpy(&(encoded_data[MESSAGE_START_INDEX]),(void *)message,message_length);

    //get localtime
    gettimeofday(&timestamp, NULL);

    //convert beaglebone 8 byte timeval to 16 byte timeval for server, if this is server this does not change anything
    timestampBeagle.tv.tv_sec=(uint64_t)timestamp.tv_sec;
    timestampBeagle.tv.tv_usec=(uint64_t)timestamp.tv_usec;

    //add timestamp to buffer
    memcpy(&(encoded_data[message_length+4]),(void *)&timestampBeagle.tv,sizeof(timestampBeagle.tv));

    calculate_checksum(encoded_data,&checksum_1,&checksum_2);

    encoded_data[length-2] = checksum_1;
    encoded_data[length-1] = checksum_2;

    return DEC_ERR_NONE;
}


Data* get_read_pointer()
{
#if DEBUG  > 1
    printf("Entering get_read_pointer\n");
#endif

    return read_data;
}

void calculate_checksum(uint8_t buffer[],uint8_t *checksum_1,uint8_t *checksum_2){
#if DEBUG  > 1
    printf("Entering calculate_checksum\n");
#endif
    int length = buffer[LENGTH_INDEX];
    *checksum_1=0;
    *checksum_2=0;

    //start byte '0x99' is not in checksum
    for (int i=1;i<length-2;i++)
    {
        *checksum_1 += buffer[i];
        *checksum_2 += *checksum_1;
    }
}

int add_timestamp(uint8_t buffer[]){
#if DEBUG  > 1
    printf("Entering add_timestamp\n");
#endif

    int length_original=buffer[1];
    uint8_t checksum_1,checksum_2;
    int new_length=length_original+16; //timeval is 16 bytes
    struct timeval tv_8;	//beaglebones timestamp is 8 byte, server timestamp is 16 byte :-(
    TimestampBeagle timestampBeagle;

    //get localtime
    gettimeofday(&tv_8, NULL);

    //convert beaglebone 8 byte timeval to 16 byte timeval for server, if this is server this does not change anything
    timestampBeagle.tv.tv_sec=(uint64_t)tv_8.tv_sec;
    timestampBeagle.tv.tv_usec=(uint64_t)tv_8.tv_usec;

    //update message length
    buffer[LENGTH_INDEX]=new_length;

    //add timestamp to buffer
    memcpy(&(buffer[length_original-2]),(void *)&timestampBeagle.tv,sizeof(timestampBeagle.tv)); //overwrite previous checksums (-2)

    //recalculate checksum
    calculate_checksum(buffer,&checksum_1,&checksum_2);
    buffer[new_length-2]=checksum_1;
    buffer[new_length-1]=checksum_2;

    return new_length;
}

int strip_timestamp(uint8_t buffer[]){
#if DEBUG  > 1
    printf("Entering strip_timestamp\n");
#endif

    int length=buffer[LENGTH_INDEX];
    uint8_t checksum_1,checksum_2;
    int new_length=length-16; //timeval is 16 bytes

    //update message length
    buffer[LENGTH_INDEX]=new_length;

    //recalculate checksum
    calculate_checksum(buffer,&checksum_1,&checksum_2);
    buffer[new_length-2]=checksum_1;
    buffer[new_length-1]=checksum_2;

    return new_length;
}

void DEC_err_handler(DEC_errCode err,void (*write_error_ptr)(char *,char *,int))  
{
#if DEBUG  > 1
    printf("Entering DEC_err_handler\n");
#endif

    static char SOURCEFILE[] = "data_decoding.c";
    //write error to local log
    switch( err ) {
    case DEC_ERR_NONE:
        break;
    case  DEC_ERR_START_BYTE:
        write_error_ptr(SOURCEFILE,"start byte is not 0x99",err);
        break;
    case DEC_ERR_CHECKSUM:
        write_error_ptr(SOURCEFILE,"wrong checksum",err);
        break;
    case DEC_ERR_UNKNOWN_BONE_PACKAGE:
        write_error_ptr(SOURCEFILE,"received unknown package from beaglebone",err);
        break;
    case DEC_ERR_UNKNOWN_LISA_PACKAGE:
        write_error_ptr(SOURCEFILE,"received unknown package from lisa",err);
        break;
    case DEC_ERR_UNKNOWN_WIND_PACKAGE:
        write_error_ptr(SOURCEFILE,"received unknown package from wind bone",err);
        break;
    case DEC_ERR_UNKNOWN_SENDER:
        write_error_ptr(SOURCEFILE,"received package from unknown sender",err);
        break;
    case DEC_ERR_LENGTH:
        write_error_ptr(SOURCEFILE,"decoded not entire package length",err);
        break;
    case DEC_ERR_UNDEFINED:
        write_error_ptr(SOURCEFILE,"undefined decoding error",err);
        break;
    default: break;
    }
}





