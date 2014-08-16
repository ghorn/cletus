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

#include "./log.h"

#include "./zmq.h"

void *context_check(void **context);
void *zsock(void *context, int type);
int zsetopt(void *sockit, int opt, int value);
const char *zprintopt(int opt);
const char *zprinttype(int type);
int zfilter(void *sockit, const char *filter);
int zgettype(void *sockit);
int zaddpath(void *sockit, const char *path);
void zsuccess(void *sockit, const char *path);
int zbind(void *sockit, const char *path);
int zconnect(void *sockit, const char *path);

inline void *context_check(void **context) {
  if (!context) {
    err("got a null void**; can't continue!");
    return NULL;
  }

  if (!(*context)) {
    /* Make the context. */
    *context = zmq_ctx_new();
  }

  if (NULL == *context) {
    err("new context failed; can't continue!");
    return NULL;
  }

  return context;
}

inline void * zsock(void *context, int type) {
  void *sockit = zmq_socket(context, type);
  if (NULL == sockit)
    zerr("couldn't get a socket");

  return sockit;
}

inline const char *zprintopt(int opt) {
  switch (opt) {
    case ZMQ_SNDHWM:
      return "SNDHWM";
    case ZMQ_RCVHWM:
      return "RCVHWM";
    case ZMQ_SNDBUF:
      return "SNDBUF";
    case ZMQ_RCVBUF:
      return "RCVBUF";
    case ZMQ_LINGER:
      return "LINGER";
    default:
      return "<unknown>";
  }
}

inline const char *zprinttype(int type) {
  switch (type) {
    case ZMQ_PUB:
      return "PUB";
    case ZMQ_SUB:
      return "SUB";
    case ZMQ_PUSH:
      return "PUSH";
    case ZMQ_PULL:
      return "PULL";
    case ZMQ_REQ:
      return "REQ";
    case ZMQ_REP:
      return "REP";
    case ZMQ_ROUTER:
      return "ROUTER";
    case ZMQ_DEALER:
      return "DEALER";
    case ZMQ_PAIR:
      return "PAIR";
    case ZMQ_XSUB:
      return "XSUB";
    case ZMQ_XPUB:
      return "XPUB";
    default:
      return "<unknown>";
  }
}

inline int zsetopt(void *sockit, int opt, int value) {
  if (value <= 0) return 0;
  const int rc = zmq_setsockopt(sockit, opt, &value, sizeof(value));
  if (rc != 0) {
    zerr("couldn't setsockopt() for '%s'", zprintopt(opt));
    return -1;
  }

  return 1;
}

inline int zfilter(void *sockit, const char *filter) {
  int rc;
  if (filter) {
    rc = zmq_setsockopt(sockit, ZMQ_SUBSCRIBE, filter, strlen(filter));
  } else {
    rc = zmq_setsockopt(sockit, ZMQ_SUBSCRIBE, NULL, 0);
  }

  if (rc != 0) {
    err("couldn't set zmq sub filter '%s': %s!",
        filter, zmq_strerror(zmq_errno()));
    return -1;
  }

  return 1;
}

inline void zdestroy(void *sockit, void *context) {
  if (sockit)
    if (zmq_close(sockit) < 0)
      zerr("closing socket");
  if (context)
    if (zmq_ctx_destroy(context) < 0)
      zerr("destroying context");
}

inline int zbind(void *sockit, const char *path) {
  if (NULL == sockit || NULL == path) return 1;
  const int rc = zmq_bind(sockit, path);
  if (rc != 0) {
    zerr("couldn't bind() to '%s'", path);
    return -1;
  }

  return 1;
}

inline int zconnect(void *sockit, const char *path) {
  if (NULL == sockit || NULL == path) return 1;
  const int rc = zmq_connect(sockit, path);
  if (rc != 0) {
    zerr("couldn't connect() to '%s'", path);
    return -1;
  }

  return 1;
}

inline int zgettype(void *sockit) {
  int ztype;
  size_t ztype_len = sizeof(ztype);
  if (zmq_getsockopt(sockit, ZMQ_TYPE, &ztype, &ztype_len) != 0) {
    zerr("couldn't getsockopt() for ZMQ_TYPE");
    return -1;
  }

  return ztype;
}

static const char *_filter = NULL;

inline void zsuccess(void *sockit, const char *path) {
  const int type = zgettype(sockit);
  switch (type) {
    case ZMQ_PUB:
      printf("successfully PUBlishing messages on '%s'.\n", path);
      break;
    case ZMQ_SUB:
      if (_filter)
        printf("successfully SUBscribed to messages on '%s'"
               " with prefix filter '%s'.\n", path, _filter);
      else
        printf("successfully SUBscribed to all messages on '%s'.\n", path);
      break;
    case ZMQ_PUSH:
      printf("successfully set up to PUSH messages on '%s'.\n", path);
      break;
    case ZMQ_PULL:
      printf("successfully set up to PULL messages on '%s'.\n", path);
      break;
    case ZMQ_REQ:
    case ZMQ_REP:
    case ZMQ_ROUTER:
    case ZMQ_DEALER:
    case ZMQ_PAIR:
    case ZMQ_XSUB:
    case ZMQ_XPUB:
      err("unsupported socket of type '%s' nevertheless reported success.",
          zprinttype(type));
    default:
      break;
  }
}

inline int zaddpath(void *sockit, const char *path) {
  const int type = zgettype(sockit);
  if (type < 0) return -1;
  int rc;
  switch (type) {
    case ZMQ_PUB:
    case ZMQ_PULL:
      rc = zbind(sockit, path);
      break;
    case ZMQ_PUSH:
    case ZMQ_SUB:
      rc = zconnect(sockit, path);
      break;
    case ZMQ_REP:
    case ZMQ_REQ:
    case ZMQ_ROUTER:
    case ZMQ_DEALER:
    case ZMQ_XSUB:
    case ZMQ_XPUB:
      err("Unsupported ZMQ socket type '%s' (%d)!", zprinttype(type), type);
      return -1;
    default:
      err("Unknown (invalid?) ZMQ socket type %d!", type);
      return -1;
  }

  if (rc < 0) {
    err("failed to add path '%s' to socket of type '%s'!\n",
        path, zprinttype(type));
    return -1;
  } else {
    zsuccess(sockit, path);
    return 1;
  }
}

inline void *setup_zmq_sender(const char *path, void **context,
                              int type, int hwm, int bufsize) {
  /* Get a new context. */
  if (context_check(context) == NULL)
    return NULL;

  /* Get a new socket. */
  void *sockit = zsock(*context, type);
  if (NULL == sockit)
    return NULL;
  /* Set options. */
  if (zsetopt(sockit, ZMQ_SNDHWM, hwm) < 0)
    goto eit;
  if (zsetopt(sockit, ZMQ_SNDBUF, bufsize) < 0)
    goto eit;
  /* Wait around for up to 2.222 seconds on a blocked socket before
   * giving up and closing it anyway. */
  if (zsetopt(sockit, ZMQ_LINGER, 100) < 0)
    goto eit;

  /* Attach to the given path. */
  if (zaddpath(sockit, path) < 0)
    goto eit;

  return sockit;

 eit:
  err("couldn't create %s socket on path '%s'!\n", zprinttype(type), path);
  zdestroy(sockit, *context);
  return NULL;
}

inline void *setup_zmq_receiver(const char *path, void **context,
                                int type, const char* filter,
                                int hwm, int bufsize) {
  if (context_check(context) == NULL)
    return NULL;
  void *sockit = zsock(*context, type);
  if (NULL == sockit)
    return NULL;
  if (zsetopt(sockit, ZMQ_RCVHWM, hwm) < 0)
    goto eit;
  if (zsetopt(sockit, ZMQ_RCVBUF, bufsize) < 0)
    goto eit;
  if (zsetopt(sockit, ZMQ_LINGER, 2222) < 0)
    goto eit;
  if (ZMQ_SUB == type) {
    _filter = filter;
    if (zfilter(sockit, filter) < 0){
      goto eit;
    _filter = NULL;
    }
  }
  if (zaddpath(sockit, path) < 0)
    goto eit;

  return sockit;

 eit:
  err("couldn't create %s socket on path '%s'!\n", zprinttype(type), path);
  zdestroy(*context, sockit);
  return NULL;
}


inline void *setup_zmq_receiver_filtered(const char *path, void **context,
                                int type, const char filter,
                                int hwm, int bufsize) {
  if (context_check(context) == NULL)
    return NULL;
  void *sockit = zsock(*context, type);
  if (NULL == sockit)
    return NULL;
  if (zsetopt(sockit, ZMQ_RCVHWM, hwm) < 0)
    goto eit;
  if (zsetopt(sockit, ZMQ_RCVBUF, bufsize) < 0)
    goto eit;
  if (zsetopt(sockit, ZMQ_LINGER, 2222) < 0)
    goto eit;
  if (ZMQ_SUB == type) {
    _filter = &filter;
    if (zfilter(sockit, &filter) < 0)
      goto eit;
    _filter = NULL;
  }
  if (zaddpath(sockit, path) < 0)
    goto eit;

  return sockit;

 eit:
  err("couldn't create %s socket on path '%s'!\n", zprinttype(type), path);
  zdestroy(*context, sockit);
  return NULL;
}

int zmq_recvm(void *sock, uint8_t *buffer, uint32_t buflen) {
  int rx = 0;
  int moar = 1;
  int ct = 0;
  while (moar) {
    const int zr = zmq_recv(sock, buffer + rx, buflen - rx, 0);
    if (zr < 0) {
      zerr("Couldn't receive chunk %d", ct);
      return -1;
    }

    rx += zr;
    size_t moarsize = sizeof(moar);
    if (zmq_getsockopt(sock, ZMQ_RCVMORE, &moar, &moarsize) < 0)
      return -1;

    if ((int) buflen - (int) rx < 0) {
      err("incoming message (%d bytes) exceeded the provided %u bytes!",
          rx, buflen);
      return -1;
    }

    ct++;
  }

  return rx;
}

int zmq_sendm(void *sock, const void *bufs[],
              const uint32_t lens[], uint8_t nbuf) {
  if (nbuf == 0) return 0;
  errno = 0;
  int zs, tx = 0;
  const int last = nbuf - 1;
  for (int i = 0; i < last; i++) {
    /* Still experimenting with this -- MP */
/*     int events; */
/*     size_t eventsize = sizeof(events); */
/*     if (zmq_getsockopt(sock, ZMQ_EVENTS, &events, &eventsize) < 0 */
/*         || !(events & ZMQ_POLLOUT)) */
/*       /\* Can't write for some reason -- try again later. *\/ */
/*       return -(i - 1); */
    if ((zs = zmq_send(sock, bufs[i], lens[i], ZMQ_SNDMORE)) !=
        (int) lens[i]) {
      zerr("Couldn't send chunk %d", i);
      return -(i - 1);
    }
    tx += zs;
  }

  if ((zs = zmq_send(sock, bufs[last], lens[last], 0)) != (int) lens[last]) {
    zerr("Couldn't send chunk %d", last);
    return -(last - 1);
  }

  tx += zs;

  return tx;
}
