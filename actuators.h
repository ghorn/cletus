#ifndef ACTUATORS_H
#define ACTUATORS_H

#define ACT_RT_PRIORITY 49
#define ACT_RT_INTERVAL 200000; /* 200us*/

void convert_for_lisa(const actuators_t* const actuators, lisa_message_t* const msg);


#endif // ACTUATORS_H
