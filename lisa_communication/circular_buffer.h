#ifndef CIRCULAR_BUFFER_H_ 
#define CIRCULAR_BUFFER_H_

#include <inttypes.h>
#include "../lisa_messages.h"

// source: wikipedia

// Opaque buffer element type.  This would be defined by the application.
typedef struct { uint8_t message[LISA_MAX_MSG_LENGTH];} ElemType;
 
/* Circular buffer object */
typedef struct {
    int         size;   /* maximum number of elements           */
    int         start;  /* index of oldest element              */
    int         end;    /* index at which to write new element  */
    ElemType   *elems;  /* vector of elements                   */
} CircularBuffer;

void cbInit(CircularBuffer *cb, int size); 
extern void cbFree(CircularBuffer *cb);
extern int cbIsFull(CircularBuffer *cb);
extern int cbIsEmpty(CircularBuffer *cb);
extern void cbWrite(CircularBuffer *cb, ElemType *elem);
extern void cbRead(CircularBuffer *cb, ElemType *elem);

#endif /*CIRCULAR_BUFFER_H_*/
