/*
 * AUTHOR: Fabian Girrbach
 */

#include <pthread.h>
#include <sched.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "uart_communication.h"
#include "log.h"
#include "circular_buffer.h"
#include "data_decoding.h"

#ifndef LOGGING
#define LOGGING 1
#endif

#ifndef DEBUG
#define DEBUG 1
#endif

#define CBSIZE 1024 * 16
#define OUTPUT_BUFFER 36
#define MAX_STREAM_SIZE 255
#define MAX_OUTPUT_STREAM_SIZE 36



/************************************
 * PROTOTYPES
 * **********************************/
static void *data_logging_lisa(void *);
static void *write_servo_commands_lisa(void *);
static void sendError(DEC_errCode err,library lib);
static void switch_cb_lisa_pointers();
static void write_uart_error(char *file_name,char *message,int err_code);
static void write_decode_error(char *file_name,char *message,int err_code);
static void write_log_error(char *file_name,char *message,int err_code);

 /***********************************
  * GLOBALS
  * *********************************/

static char FILENAME[] = "main_bealgebone.c";




#if LOGGING > 0
//log ping pong buffers for data from lisa
static CircularBuffer cb_data_lisa_ping;
static CircularBuffer cb_data_lisa_pong;
static CircularBuffer *cb_read_lisa = &cb_data_lisa_ping;
static CircularBuffer *cb_write_lisa = &cb_data_lisa_pong;
static int reading_flag_lisa=0;



#endif

//function pointer to write errors to log
void (*write_uart_error_ptr)(char *,char *,int);
void (*write_decode_error_ptr)(char *,char *,int);
void (*write_log_error_ptr)(char *,char *,int);
void (*write_error_ptr)(char *,char *,int);

 /***********************************
  * MAIN
  * *********************************/


int main(int argc, char *argv[]){
# if DEBUG > 0
    printf("Entering in main with arguments: \n %30s \n",*argv );
#endif
    write_uart_error_ptr = &write_uart_error;  //initialize the function pointer to write error
    write_decode_error_ptr = &write_decode_error;
    write_log_error_ptr = &write_log_error;



    //init log (mount sd card if necessary)
# if DEBUG > 0
    printf("Initialize Logfile. \n" );
#endif
    int err = init_log();
    LOG_err_handler(err,write_log_error_ptr);

    if(err != LOG_ERR_NONE){
        exit(EXIT_FAILURE);		//mounting SD card failed
    }


    //init the data decode pointers
    init_decoding();
# if DEBUG > 0
     printf("Initialize Buffers. \n" );
#endif
    //init circular data log buffers
     cbInit(cb_read_lisa, CBSIZE);
     cbInit(cb_write_lisa, CBSIZE);
# if DEBUG > 0
     printf("Initialize Serial Port. \n" );
#endif
    err = serial_port_setup();
    UART_err_handler(err,write_uart_error_ptr);
    if(err != UART_ERR_NONE){
        exit(EXIT_FAILURE);
    }

    //thread variables
    pthread_t thread_data_logging_lisa, thread_servo_commands_writing_lisa;



    //create a thread which executes data_logging_lisa
    if(pthread_create(&thread_data_logging_lisa, NULL, data_logging_lisa,NULL)) {
        error_write(FILENAME,"error creating lisa logging thread");
        exit(EXIT_FAILURE);
    }


    //create a thread which executes data_logging_lisa
    if(pthread_create(&thread_servo_commands_writing_lisa, NULL, write_servo_commands_lisa,NULL)) {
        error_write(FILENAME,"error creating lisa writing servo thread");
        exit(EXIT_FAILURE);
    }




    int message_length;
    ElemType cb_elem = {{0}};
    uint8_t input_buffer[INPUT_BUFFER_SIZE];


    while(1)
    {
        message_length = serial_input_get_lisa_data(input_buffer); //blocking !!!
        if(message_length > 0){

#if DEBUG > 0
            printf("Received serial input with message id: %c\n", input_buffer[3]);
#endif


            //add timestamp
            message_length=add_timestamp(input_buffer);

            //Decode messages to see what we receive
            int err_decode = data_decode(input_buffer);
            if (err_decode != DEC_ERR_NONE)
            {
                printf("Error while decoding data messages.");
            }



            //write the data to circular buffer for log thread
             if(!cbIsFull(cb_write_lisa)){
                 memcpy (&cb_elem.value, input_buffer, message_length);
                 cbWrite(cb_write_lisa, &cb_elem);
             }else{
                if(reading_flag_lisa==0){
                    switch_cb_lisa_pointers();
                    //printf("switching lisa pointers\n");
                }else{
                    printf("LISA WRITE WAS NOT READY \n");
                    exit(1); //FOR DEBUGGING
                }
             }

        }else{
        //send error message to server: not receiving data on uart port
            printf("error on uart, see log...\n"); //FOR DEBUGGING
            UART_err_handler(message_length,write_uart_error_ptr);
        }

    }

    UART_err_handler(serial_port_close(),write_uart_error_ptr);



    //wait for the logging thread to finish
    if(pthread_join(thread_data_logging_lisa, NULL)) {
        error_write(FILENAME,"error joining thread_data_logging_lisa");
    }


    //free circular buffers
    cbFree(cb_read_lisa);
    cbFree(cb_write_lisa);


    return 0;
}

/************************************
 * FUNCTIONS
 * **********************************/


//Function for writing data messages from buffer to the lisa log file
static void *data_logging_lisa(void *arg){
/*-------------------------START OF THREAD: LISA LOGGING------------------------*/

    ElemType cb_elem = {{0}};
    LOG_err_handler(open_data_lisa_log(),write_log_error_ptr);

    while(1){
        //Check if something is in buffer and write it to file
        if (!cbIsEmpty(cb_read_lisa)) {
            reading_flag_lisa=1;
            cbRead(cb_read_lisa, &cb_elem);
            LOG_err_handler(write_data_lisa_log(cb_elem.value,cb_elem.value[1]),write_log_error_ptr);
            usleep(100);
        }else{
            reading_flag_lisa=0;
            usleep(1000);
        }
    }
    LOG_err_handler(close_data_lisa_log(),write_log_error_ptr);

    return NULL;
/*-------------------------END OF THREAD: LISA LOGGING------------------------*/
}


static void *write_servo_commands_lisa(void *arg){
/*-------------------------START OF THREAD: WRITE SERVO COMMANDS------------------------*/

    //1. read data from i don't now where
    uint8_t encoded_data[MAX_OUTPUT_STREAM_SIZE];

    int i=0;

    while(1)
    {

        Output output;

        //create test data
        output.message.servo_1=-i;
        output.message.servo_2=i;
        output.message.servo_3=i;
        output.message.servo_4=i;
        output.message.servo_5=i;
        output.message.servo_6=i;
        output.message.servo_7=0;
        i=i+2;
        if(i>12800){
            i=0;
        }
        printf("Current Value: %i",i);

        //2. encode the data
        DEC_err_handler(data_encode(output.raw,sizeof(output.raw),encoded_data,SERVER,SERVO_COMMANDS),write_error_ptr);

        //3. send data over UART
        int new_length = strip_timestamp(encoded_data); //lisa expects a package without a timestamp

        UART_err_handler(serial_port_write(encoded_data,new_length),write_uart_error_ptr);

        //usleep(20000); //60 hz

        sleep(1);
    }



    return NULL;
/*------------------------END OF SECOND THREAD------------------------*/
}

static void sendError(DEC_errCode err,library lib){

//        //int message_length;
//        uint8_t encoded_data[MAX_STREAM_SIZE];
//        //Data data;
//        Beagle_error error_message;

//        //encode an error package
//        error_message.library=lib;
//        error_message.error_code=err;
//        data_encode((uint8_t *)&error_message,sizeof(error_message),encoded_data,2,2);
//        //message_length=sizeof(encoded_data);

//        printf("Error handlers invoked sending of error message! \n Code: %i" ,error_message.error_code );
}



static void switch_cb_lisa_pointers(){
        CircularBuffer *temp = cb_read_lisa;
        cb_read_lisa = cb_write_lisa;
        cb_write_lisa = temp;
}


static void write_uart_error(char *file_name,char *message,int err_code)
{
    //TODO: make it thread safe!!
    error_write(file_name,message);
    sendError(err_code,UART_L);
}



static void write_decode_error(char *file_name,char *message,int err_code)
{
    //TODO: make it thread safe!!
    error_write(file_name,message);
    sendError(err_code,DECODE_L);
}

static void write_log_error(char *file_name,char *message,int err_code)
{
    //TODO: make it thread safe!!
    error_write(file_name,message);
    sendError(err_code,LOG_L);
}

