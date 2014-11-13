#ifndef FIFO_H
#define FIFO_H

typedef struct {
     char * buf;
     int head;
     int tail;
     int size;
} fifo_t;

int fifo_write(fifo_t * f, const void * buf, int nbytes);
int fifo_read(fifo_t * f, void * buf, int nbytes);
void fifo_init(fifo_t * f, int size);



#endif // FIFO_H
