/* Copyright 2014 Greg Horn <gregmainland@gmail.com>
 *
 * This file is hereby placed in the public domain, or, if your legal
 * system does not recognize such a concept, you may consider it
 * licensed under BSD 3.0.  Use it for good.
 */

#ifndef __CONTROLLER_H__
#define __CONTROLLER_H__
#include "./structures.h"

void run_controller(const sensors_t * const y, actuators_t * const u);

#endif // __CONTROLLER_H__
