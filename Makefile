# What's the executable called?
PROJ = run_sensors run_controller run_actuators

# What C or C++ files must we compile to make the executable?
C_SRC = run_sensors.c \
        run_controller.c \
        run_actuators.c \
        controller.c \
        sensors.c \
        misc.c \
        zmq.c

CXX_SRC = \
#	main.cpp \
#	parsing.cpp

LIBS = -lzmq -lm

Q ?= @

# build in the autopilot
# this has a makefile format to make it easy to include
# into the paparazzi build system
UNAME := $(shell uname)

LDFLAGS = -lm -lzmq
INCLUDES =

#CFLAGS = -O3 -Wall -Wextra -Werror -std=gnu99 -Wimplicit -Wshadow -Wswitch-default -Wswitch-enum -Wundef -Wuninitialized -Wpointer-arith -Wstrict-prototypes -Wmissing-prototypes -Wcast-align -Wformat=2 -Wimplicit-function-declaration -Wredundant-decls -Wformat-security  -Werror -Os -march=native -ftree-vectorize -flto -fPIC -D_FORTIFY_SOURCE=2 -fstack-protector-all -fno-strict-overflow   -g -ftrapv

ifeq ($(UNAME),Darwin)
	LDFLAGS += -L/opt/local/lib
	INCLUDES += -I/opt/local/include
	INCLUDES += -isystem /usr/local/include
endif


OBJ = $(C_SRC:%.c=%.o) $(CXX_SRC:%.cpp=%.o)

## Compile pedantically and save pain later
CXX_WARNINGFLAGS = -Wall -Wextra -Wshadow -Werror
C_WARNINGFLAGS =  -Wall -Wextra -Wshadow -Werror -Wstrict-prototypes
C_WARNINGFLAGS += -Wimplicit -Wswitch-default -Wswitch-enum -Wundef -Wuninitialized -Wpointer-arith -Wstrict-prototypes -Wmissing-prototypes -Wcast-align -Wformat=2 -Wimplicit-function-declaration -Wredundant-decls -Wformat-security  -Werror -Os -march=native -ftree-vectorize -flto -fPIC -D_FORTIFY_SOURCE=2 -fstack-protector-all -fno-strict-overflow -ftrapv
DEBUGFLAGS ?= -g -DDEBUG # -pg to generate profiling information

OPTFLAGS = -O3

CFLAGS ?= $(C_WARNINGFLAGS) $(DEBUGFLAGS) $(FEATUREFLAGS) $(INCLUDES) $(OPTFLAGS) -std=gnu99
CXXFLAGS ?= $(CXX_WARNINGFLAGS) $(DEBUGFLAGS) $(FEATUREFLAGS) $(INCLUDES) $(OPTFLAGS) -std=gnu++0x
CC ?= gcc
CXX ?= g++

.PHONY: all
all: $(PROJ)

.SECONDEXPANSION:
$(PROJ): % : $$(findstring $$(*:%=%.o),$(OBJ)) $(filter-out $(PROJ:%=%.o),$(OBJ))
	@echo LD $@
ifneq (,$(CXX_SRC))
	$(Q)$(CXX) $(filter %.o %.a %.so, $^) $(LDFLAGS) -o $@
else
	$(Q)$(CC) $(filter %.o %.a %.so, $^) $(LDFLAGS) -o $@
endif


%.o : %.c
	@echo CC $@
	$(Q)$(CC) $(CFLAGS) -c $< -o $@

%.o : %.cpp
	@echo CXX $@
	$(Q)$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -f $(PROJ)
	rm -f $(OBJ)
