#ifndef ACTUATORS_H
#define ACTUATORS_H
#include "structures.h"
#include "./protos_c/messages.pb-c.h"

#define ACT_RT_PRIORITY 49
#define ACT_RT_INTERVAL 200000000; /* 200us*/

void convert_for_lisa(const Protobetty__Actuators* const actuators, lisa_actuators_t* const msg);
void calculate_checksum(uint8_t buffer[],uint8_t *checksum_1,uint8_t *checksum_2);


#endif // ACTUATORS_H
