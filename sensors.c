/* Copyright 2014 Greg Horn <gregmainland@gmail.com>
 *
 * This file is hereby placed in the public domain, or, if your legal
 * system does not recognize such a concept, you may consider it
 * licensed under BSD 3.0.  Use it for good.
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <math.h>

#include "./sensors.h"


///
/// \brief scaled_to_protobuf helper function to copy raw imu data to protbuf message
/// \param source struct with raw values from lisa
/// \param dest protobuf message where data should be copied to
///
void raw_to_protobuf(const xyz_int *const source, Protobetty__Xyz *dest)
{
  dest->x = (double)source->x;
  dest->y = (double)source->y;
  dest->z = (double)source->z;
#ifdef DEBUG
//  printf("\n X: %f ;\n Y: %f ;\n Z: %f \n",dest->x,dest->y,dest->z);
#endif
}

///
/// \brief scaled_to_protobuf helper function to copy scaled imu data to protbuf message
/// \param source struct with raw values from lisa
/// \param dest protobuf message where data should be copied to
/// \param coef cofficent for scaling value to unit
///
void scaled_to_protobuf(const xyz_int *const source, Protobetty__Xyz *dest, double coef)
{
  dest->x = (double)source->x * coef;
  dest->y = (double)source->y * coef;
  dest->z = (double)source->z * coef;
#ifdef DEBUG
 // printf("\n X: %f ;\n Y: %f ;\n Z: %f \n",dest->x,dest->y,dest->z);
#endif
}

///
/// \brief copy_timestamp helper function to copy timestampf from lisa to protobuf
/// \param source lisa message
/// \param dest protobuf message
///
void copy_timestamp(const timestamp_t *const source, Protobetty__Timestamp *dest)
{
   dest->tnsec = source->tnsec;
   dest->tsec = source->tsec;
}


///
/// \brief Convert integer to character array
/// \param number to convert
/// \return char pointer
///
char * toArray(int number)
    {
        int n = log10(number) + 1;
        int i;
      char *numberArray = calloc(n, sizeof(char));
        for ( i = 0; i < n; ++i, number /= 10 )
        {
            numberArray[i] = number % 10;
        }
        return numberArray;
    }

