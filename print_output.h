#ifndef PRINT_OUTPUT_H
#define PRINT_OUTPUT_H

#include "./zmq.h"


void send_printf(void* zmq_sockit, char *message, int type, char *tag);
void send_debug(void* zmq_sockit, char *tag, const char* const format, ...);
void send_error(void* zmq_sockit, char *tag, const char* const format, ...);
void send_warning(void* zmq_sockit, char *tag, const char* const format, ...);
void send_info(void* zmq_sockit, char *tag, const char* const format, ...);

//*******************************
// MACROS for Console output
//*******************************
#define ANSI_COLOR_RED     "\x1b[31m"
#define ANSI_COLOR_GREEN   "\x1b[32m"
#define ANSI_COLOR_YELLOW  "\x1b[33m"
#define ANSI_COLOR_BLUE    "\x1b[34m"
#define ANSI_COLOR_MAGENTA "\x1b[35m"
#define ANSI_COLOR_CYAN    "\x1b[36m"
#define ANSI_COLOR_RESET   "\x1b[0m"
#define LOG_INFO(X) printf("%s %s %s",ANSI_COLOR_CYAN,X,ANSI_COLOR_RESET)
#define LOG_ERROR(X) printf("%s %s %s",ANSI_COLOR_RED,X,ANSI_COLOR_RESET)
#define LOG_WARNING(X) printf("%s %s %s",ANSI_COLOR_YELLOW,X,ANSI_COLOR_RESET)


#endif // PRINT_OUTPUT_H
