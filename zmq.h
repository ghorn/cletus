/* Copyright 2014 Matt Peddie <peddie@alum.mit.edu>
 *
 * This file is hereby placed in the public domain, or, if your legal
 * system does not recognize such a concept, you may consider it
 * licensed under BSD 3.0.  Use it for good.
 */
#ifndef __FREIBURG_ZMQ_H__
#define __FREIBURG_ZMQ_H__

#include <stdint.h>
#include <zmq.h>

/* These functions return either a ZMQ socket (void *) or an integer
 * code (int) telling how many bytes were transmitted or received, or
 * a value <0 if the function fails. */

/* Destroy some ZMQ stuff.  If this isn't the last socket in this
 * context to be cleaned up, just pass NULL as the context. */
void zdestroy(void *sockit, void *context);

/* Sets up a ZMQ PUSH or PUB socket. */
void *setup_zmq_sender(const char *path, void **context, int type,
                       int hwm, int bufsize);

/* Sets up a ZMQ PULL or SUB socket.  If you use a PULL socket, then
 * 'filter' doesn't matter.  For a SUB socket, "" matches all
 * messages. */
void *setup_zmq_receiver(const char *path, void **context,
                         int type, const char *filter,
                         int hwm, int bufsize);
void *setup_zmq_receiver_filtered(const char *path, void **context,
                         int type, const char filter,
                         int hwm, int bufsize);
/* Receive a potentially multi-part ZMQ message on the given socket
 * into the 'buffer' of the specified length 'buflen'.  If the message
 * is too big, an error is returned. */
int zmq_recvm(void *sock, uint8_t *buffer, uint32_t buflen);

/* Send a multi-part ZMQ message on the given socket.  'bufs' is an
 * array of pointers to message parts you want to send (in order);
 * 'lens' contains their lengths.  'nbuf' specifies how many parts
 * make up the message. */
int zmq_sendm(void *sock, const void *bufs[],
              const uint32_t lens[], uint8_t nbuf);

#endif  /* __FREIBURG_ZMQ_H__ */
