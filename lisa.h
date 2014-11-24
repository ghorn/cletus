#ifndef LISA_H
#define LISA_H

int lisa_open_connection(void);
void lisa_close_connection(void);
int lisa_read_message(int* exit);
int lisa_init_message_processing(unsigned char* buffer);
int lisa_flush_buffers(void);


#endif // LISA_H
