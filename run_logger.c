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
#include <stdio.h>
#include <sys/types.h>
#include <sys/mman.h>

#include "./zmq.h"
#include "./comms.h"
#include "./structures.h"
#include "./log.h"
#include "./misc.h"
#include "./print_output.h"

#include "./controller.h"
#include "./protos_c/messages.pb-c.h"


static long safe_to_file(void);


/* ZMQ resources */
static void *zctx = NULL;
static void *zsock_logs = NULL;
void *zsock_print = NULL;
//pointer to temporal memory in ram
uint8_t *ptr_temp_memory;
//counter of elements received
static uint64_t counter_log_messages = 0;
//maximum limit of logs
const uint64_t NUMBER_OF_LOGS = 10000;
char* TAG = "RUN_LOGGER";
Protobetty__LogMessage **log_messages = NULL;



/* Error tracking. */
int txfails = 0, rxfails = 0;

static void __attribute__((noreturn)) die(int code) {

    send_info(zsock_print,TAG,"Starting Logging now....");
    safe_to_file();
    send_info(zsock_print,TAG,"Finished Logging");

    free_workbuf(ptr_temp_memory, NUMBER_OF_LOGS*PROTOBETTY__MESSAGE__CONSTANTS__MAX_MESSAGE_SIZE);
    zdestroy(zsock_print, NULL);
    zdestroy(zsock_logs, NULL);
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


    struct timespec t;
    struct sched_param param;
    int rt_interval= 0;
    printf("%i", argc);
    if (argc == 3)
    {
        char* arg_ptr;
        long priority = strtol(argv[1], &arg_ptr,10);
        if (*arg_ptr != '\0' || priority > INT_MAX) {
            printf("Failed to read passed priority. Using DEFAULT value instead.\n");
            priority = DEFAULT_RT_PRIORITY;

        }
        printf("Setting priority to %li\n", priority);
        set_priority(&param, priority);

        long frequency = strtol(argv[2], &arg_ptr,10);
        if (*arg_ptr != '\0' || frequency > INT_MAX) {
            printf("Failed to read passed frequency. Using DEFAULT value instead.\n");
            frequency = DEFAULT_RT_FREQUENCY;
        }
        printf("Setting frequency to %li Hz.\n", frequency);
        rt_interval = (NSEC_PER_SEC/frequency);
    }
    else
    {
        printf("No paarameters passed. Using DEFAULT values: \nPRIORITY=%i and FREQUENCY=%i\n",
               DEFAULT_RT_PRIORITY, DEFAULT_RT_FREQUENCY);
        set_priority(&param, DEFAULT_RT_PRIORITY);
        rt_interval = (NSEC_PER_SEC/DEFAULT_RT_FREQUENCY);
    }
    stack_prefault();






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


    zsock_logs = setup_zmq_receiver(LOG_CHAN, &zctx, ZMQ_PULL, NULL, 1000, 500);
    if (NULL == zsock_logs)
        die(1);
    zsock_print = setup_zmq_sender(PRINT_CHAN, &zctx, ZMQ_PUSH, 100, 500);
    if (NULL == zsock_print)
        die(1);




    zmq_pollitem_t poll_logs = {
            .socket=zsock_logs,
            .fd=-1,
            .events= ZMQ_POLLIN,
            .revents=0
    };



    ptr_temp_memory = alloc_workbuf(NUMBER_OF_LOGS*PROTOBETTY__MESSAGE__CONSTANTS__MAX_MESSAGE_SIZE);
    if (ptr_temp_memory == NULL)
    {
        printf("Error allocating memory!\n");
        die(1);
    }
    log_messages = alloc_workbuf(sizeof(Protobetty__LogMessage*)*NUMBER_OF_LOGS);
    if (log_messages == NULL)
    {
        printf("Error allocating memory!\n");
        die(1);
    }

    uint64_t byte_counter = 0;

    clock_gettime(CLOCK_MONOTONIC ,&t);
    /* start after one second */
    t.tv_sec++;

    /* Here's the main loop -- we only do stuff when input or output
       * happens.  The timeout can be put to good use, or you can also use
       * timerfd_create() to create a file descriptor with a timer under
       * the hood and dispatch on that.
       *
       * I apologize for the length of this loop.  For production code,
       * you'd want to pull most of the actual handling out into functions
       * and simply loop over your polls; I've left it all inline here
       * mostly out of laziness. */
    for (;;)
    {
        if (bail) die(bail);

        /* wait until next shot */
        clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME, &t, NULL);
        /* Poll for activity; time out after 10 milliseconds. */
        const int polled = zmq_poll(&poll_logs, 1, 1000);
        if (polled < 0) {
            if (bail) die(bail);
            zerr("while polling");
            calc_next_shot(&t,rt_interval);
            continue;
        } else if (polled == 0) {
            if (bail) die(bail);
            {
                printf("No messages in queue \n");
                calc_next_shot(&t,rt_interval);
                continue;
            }
        }



        if (bail) die(bail);
        if ((counter_log_messages) < NUMBER_OF_LOGS)
        {
            if (poll_logs.revents & ZMQ_POLLIN)
            {
                const int zmq_received = zmq_recvm(zsock_logs,
                                                   (uint8_t*) &ptr_temp_memory[byte_counter],
                                                   PROTOBETTY__MESSAGE__CONSTANTS__MAX_MESSAGE_SIZE);
                if (zmq_received > 0)
                {
                    log_messages[counter_log_messages] = protobetty__log_message__unpack(NULL, zmq_received, &ptr_temp_memory[byte_counter]);
                    byte_counter += zmq_received;
                    counter_log_messages++;
                }
                /* Clear the poll state. */
                poll_logs.revents = 0;
            }
        }
        else
        {
            //stop polling for new messages beacuse we reached limit
            poll_logs.events = 0;
        }
        calc_next_shot(&t,rt_interval);
    }
    /* Shouldn't get here. */
    return 0;
}

static long safe_to_file(void)
{
    FILE *ptr_myfile;
    //Open file
    timestamp_t timestamp;
    gettime(&timestamp);
    char filename[50];
    snprintf(filename, sizeof(filename),"%"PRIu64"_logdata.bin",timestamp.tsec);
    ptr_myfile=fopen(filename,"wb");
    if (!ptr_myfile)
    {
        printf("Unable to open file!");
        return -1;
    }
    //Allocate protobuf structure for sensors and set data
    Protobetty__LogContainer log_container = PROTOBETTY__LOG_CONTAINER__INIT;
    log_container.n_log_data = counter_log_messages;
    log_container.log_data = log_messages;
    // back it to buffer
    const uint64_t packed_size = protobetty__log_container__get_packed_size(&log_container);
    uint8_t* buffer = alloc_workbuf(packed_size);
    protobetty__log_container__pack(&log_container,buffer);
    //write bytewise data to file
    send_debug(zsock_print, TAG, "Writing data to file ...\n");
    printf("Writing data to file ...");

    for (uint64_t i = 0; i < packed_size; i++)
    {
        fwrite(&buffer[i], sizeof(uint8_t), 1, ptr_myfile);
        printf(".");
    }
    send_debug(zsock_print, TAG, "Finsihed writing. Closing File.");
    printf("Finsihed writing. Closing File.\n");
    free_workbuf(buffer,packed_size);

    fclose(ptr_myfile);

    return packed_size;

}


