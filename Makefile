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
        protos_cpp/messages.pb.cc
#	main.cpp \
#	parsing.cpp

LIBS = $(shell pkg-config --libs libzmq) -lm -lrt -lprotobuf

Q ?= @

PROTOS = protos_cpp/messages.pb.cc \
         protos_cpp/messages.pb.h

HS_PROTOS = hs/src/Messages/Rc.hs \
            hs/src/Messages/Mode3.hs \
            hs/src/Messages/UpDown.hs \
            hs/src/Messages.hs

# build in the autopilot
# this has a makefile format to make it easy to include
# into the paparazzi build system
UNAME := $(shell uname)

LDFLAGS = $(LIBS)
INCLUDES =

#CFLAGS = -O3 -Wall -Wextra -Werror -std=gnu99 -Wimplicit -Wshadow -Wswitch-default -Wswitch-enum -Wundef -Wuninitialized -Wpointer-arith -Wstrict-prototypes -Wmissing-prototypes -Wcast-align -Wformat=2 -Wimplicit-function-declaration -Wredundant-decls -Wformat-security  -Werror -Os -march=native -ftree-vectorize -flto -fPIC -D_FORTIFY_SOURCE=2 -fstack-protector-all -fno-strict-overflow   -g -ftrapv

ifeq ($(UNAME),Darwin)
	LDFLAGS += -L/opt/local/lib
	INCLUDES += -I/opt/local/include
	INCLUDES += -isystem /usr/local/include
endif


OBJ = $(C_SRC:%.c=%.o) $(CXX_SRC:%.cpp=%.o) $(CXX_SRC:%.cc=%.o)

## Compile pedantically and save pain later
CXX_WARNINGFLAGS = -Wall -Wextra -Wshadow
C_WARNINGFLAGS =  -Wall -Wextra -Wshadow -Wstrict-prototypes
C_WARNINGFLAGS += -Wimplicit -Wswitch-default -Wswitch-enum -Wundef -Wuninitialized -Wpointer-arith -Wstrict-prototypes -Wmissing-prototypes -Wcast-align -Wformat=2 -Wimplicit-function-declaration -Wredundant-decls -Wformat-security -march=native -ftree-vectorize -flto -fPIC -D_FORTIFY_SOURCE=2 -fstack-protector-all -fno-strict-overflow -ftrapv

C_WARNINGFLAGS += -Werror
CXX_WARNINGFLAGS += -Werror
DEBUGFLAGS ?= -g -DDEBUG # -pg to generate profiling information

OPTFLAGS = -O3

CFLAGS ?= $(C_WARNINGFLAGS) $(DEBUGFLAGS) $(FEATUREFLAGS) $(INCLUDES) $(OPTFLAGS) -std=gnu99
CXXFLAGS ?= $(CXX_WARNINGFLAGS) $(DEBUGFLAGS) $(FEATUREFLAGS) $(INCLUDES) $(OPTFLAGS) -std=gnu++0x
CC ?= gcc
CXX ?= g++

HS_STRUCTS = hs/src/Structs/Structures.hs hs/src/Structs/Structures.hsc

.PHONY: all
all: $(PROJ) $(PROTOS)
hs: $(HS_STRUCTS) $(HS_PROTOS)

.SECONDEXPANSION:
$(PROJ): % : $$(findstring $$(*:%=%.o),$(OBJ)) $(filter-out $(PROJ:%=%.o),$(OBJ))
	@echo LD $@
ifneq (,$(CXX_SRC))
	$(Q)$(CXX) $(filter %.o %.a %.so, $^) $(LDFLAGS) -o $@
else
	$(Q)$(CC) $(filter %.o %.a %.so, $^) $(LDFLAGS) -o $@
endif

$(HS_PROTOS) : messages.proto
	@echo hprotoc $<
	$(Q)hprotoc --haskell_out=hs/src $<

$(PROTOS) : messages.proto
	@echo protoc $<
	$(Q)protoc --cpp_out=protos_cpp $<

sim/src/Structs/Structures.hsc : structures.h
	@echo c2hsc $@
	$(Q)cd sim/src/Structs && c2hsc --prefix=Structs ../../../$<

sim/src/Structs/Structures.hs : sim/src/Structs/Structures.hsc
	@echo hsc2hs $@
	$(Q)hsc2hs -Isim/src/Structs $< -o $@

%.o : %.c
	@echo CC $@
	$(Q)$(CC) $(CFLAGS) -c $< -o $@

%.o : %.cpp
	@echo CXX $@
	$(Q)$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -f $(PROJ)
	rm -f $(PROTOS)
	rm -f $(HS_PROTOS)
	rm -f protos_cpp/messages.pb.o
	rm -f $(OBJ)
	rm -f $(HS_STRUCTS)
