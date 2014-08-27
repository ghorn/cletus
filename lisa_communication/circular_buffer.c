/*
 * circular buffer source: wikipedia
 */

/* Circular buffer example, keeps one slot open */
 
#include <stdlib.h>
#include "circular_buffer.h"


#ifndef DEBUG 
#define DEBUG 0
#endif

#ifdef DEBUG
#include <stdio.h>
#endif
 
void cbInit(CircularBuffer *cb, int size) {
    #ifdef DEBUG
		printf("Entering cbInit\n");
	#endif
	
    cb->size  = size + 1; /* include empty elem */
    cb->start = 0;
    cb->end   = 0;
    cb->elems = (ElemType *)calloc(cb->size, sizeof(ElemType));
}
 
void cbFree(CircularBuffer *cb) {
    #ifdef DEBUG
		printf("Entering cbFree\n");
	#endif
	
    free(cb->elems); /* OK if null */ }
 
int cbIsFull(CircularBuffer *cb) {
    #ifdef DEBUG
		printf("Entering cbIsFull\n");
	#endif
	
    return (cb->end + 1) % cb->size == cb->start; }
 
int cbIsEmpty(CircularBuffer *cb) {
//    #ifdef DEBUG
//		printf("Entering cbIsEmpty\n");
//	#endif
	
    return cb->end == cb->start; }
 
/* Write an element, overwriting oldest element if buffer is full. App can
   choose to avoid the overwrite by checking cbIsFull(). */
void cbWrite(CircularBuffer *cb, ElemType *elem) {
//    #ifdef DEBUG
//		printf("Entering cbWrite\n");
//	#endif
	
    cb->elems[cb->end] = *elem;
    cb->end = (cb->end + 1) % cb->size;
    if (cb->end == cb->start)
        cb->start = (cb->start + 1) % cb->size; /* full, overwrite */
}
 
/* Read oldest element. App must ensure !cbIsEmpty() first. */
void cbRead(CircularBuffer *cb, ElemType *elem) {
//    #ifdef DEBUG
//		printf("Entering cbRead\n");
//	#endif
	
    *elem = cb->elems[cb->start];
    cb->start = (cb->start + 1) % cb->size;
}
 
