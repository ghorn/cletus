#include "print_output.h"
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <stdarg.h>
#include <string.h>
#include "./misc.h"
#include "./protos_c/messages.pb-c.h"


const uint16_t MAX_PRINT_MSG_SIZE = 256;


void send_printf(void* zmq_sockit,char * toprint, int type, char* tag)
{
    Protobetty__Printf message = PROTOBETTY__PRINTF__INIT;
    Protobetty__Timestamp timestamp = PROTOBETTY__TIMESTAMP__INIT;
    //get current timestamp
    get_protbetty_timestamp(&timestamp);
    //get string to print


    //assign data to message
    message.message = toprint;
    message.timestamp = &timestamp;
    message.type = type;
    if (tag != NULL)
        message.tag = tag;

    //
    uint8_t zmq_buffer[MAX_PRINT_MSG_SIZE];
    int length = protobetty__printf__get_packed_size(&message);
    protobetty__printf__pack(&message, &zmq_buffer[0]);
    zmq_send(zmq_sockit,zmq_buffer,length,0);

}


void send_debug(void* zmq_sockit, char * tag, const char* const format, ...)
{
    char buffer[MAX_PRINT_MSG_SIZE];
    va_list args;
    va_start( args, format );
    vsprintf(buffer,format,args);
    va_end( args );
    send_printf(zmq_sockit,buffer,PROTOBETTY__PRINTF__TYPE__DEBUG, tag);
}

void send_error(void* zmq_sockit, char * tag, const char* const format, ...)
{
    char buffer[MAX_PRINT_MSG_SIZE];
    va_list args;
    va_start( args, format );
    vsprintf(buffer,format,args);
    va_end( args );    send_printf(zmq_sockit,buffer,PROTOBETTY__PRINTF__TYPE__ERROR, tag);
}

void send_warning(void* zmq_sockit,char * tag,const char* const format, ...)
{
    char buffer[MAX_PRINT_MSG_SIZE];
    va_list args;
    va_start( args, format );
    vsprintf(buffer,format,args);
    va_end( args );
    send_printf(zmq_sockit,buffer,PROTOBETTY__PRINTF__TYPE__WARNING, tag);
}

void send_info(void* zmq_sockit,char * tag,const char* const format, ...)
{
    char buffer[MAX_PRINT_MSG_SIZE];
    va_list args;
    va_start( args, format );
    vsprintf(buffer,format,args);
    va_end( args );
    send_printf(zmq_sockit,buffer,PROTOBETTY__PRINTF__TYPE__INFO,tag);
}


