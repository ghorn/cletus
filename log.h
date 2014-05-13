/* Copyright 2014 Matt Peddie <peddie@alum.mit.edu>
 *
 * This file is hereby placed in the public domain, or, if your legal
 * system does not recognize such a concept, you may consider it
 * licensed under BSD 3.0.  Use it for good.
 */
#ifndef __LOG_H__
#define __LOG_H__

/* Log an error. */
#define err(str, ...)                                           \
  fprintf(stderr, "%s:line %d in %s(): error: " str "\n",       \
          __FILE__, __LINE__, __func__, ##__VA_ARGS__)

/* Log a ZMQ error. */
#define zerr(str, ...)                                   \
  err("(zmq): " str ": %s\n", ##__VA_ARGS__, zmq_strerror(zmq_errno()))

/* Log an error via errno. */
#define serr(str, ...)                                   \
  err("(errno): " str ": %s\n", ##__VA_ARGS__, strerror(errno))


#endif  /* __LOG_H__ */
