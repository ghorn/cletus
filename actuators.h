#ifndef ACTUATORS_H
#define ACTUATORS_H

#define RT_PRIORITY 49
#define RT_INTERVAL 200000; /* 200us*/

void convert_for_lisa(const actuators_t* const actuators, lisa_message_t* const msg);


#endif // ACTUATORS_H
