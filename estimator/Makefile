# What's the executable called?
PROJ = estimator

# What C or C++ files must we compile to make the executable?
C_SRC =

CXX_SRC = estimator.cpp \
	system.cpp \
	estmath.cpp
#	main.cpp \
#	parsing.cpp

LIBS = -lzmq -lm

Q ?= @

UNAME := $(shell uname)

LDFLAGS = $(LIBS)
INCLUDES =

ifeq ($(UNAME),Darwin)
	LDFLAGS += -L/opt/local/lib
	INCLUDES += -I/opt/local/include
	INCLUDES += -isystem /usr/local/include
endif

OBJ = $(C_SRC:%.c=%.o) $(CXX_SRC:%.cpp=%.o)

## Compile pedantically and save pain later
CXX_WARNINGFLAGS = -Wall -Wextra -Wshadow #-Wno-unused-parameter -Wno-unused-variable
C_WARNINGFLAGS =  -Wall -Wextra -Wshadow -Wstrict-prototypes
C_WARNINGFLAGS += -Wimplicit -Wswitch-default -Wswitch-enum -Wundef -Wuninitialized -Wpointer-arith -Wstrict-prototypes -Wmissing-prototypes -Wcast-align -Wformat=2 -Wimplicit-function-declaration -Wredundant-decls -Wformat-security -march=native -ftree-vectorize -flto -fPIC -D_FORTIFY_SOURCE=2 -fstack-protector-all -fno-strict-overflow -ftrapv

#C_WARNINGFLAGS += -Werror
#CXX_WARNINGFLAGS += -Werror
DEBUGFLAGS ?= -g -DDEBUG # -pg to generate profiling information

OPTFLAGS = -O3

CFLAGS ?= $(C_WARNINGFLAGS) $(DEBUGFLAGS) $(FEATUREFLAGS) $(INCLUDES) $(OPTFLAGS) -std=gnu99
CXXFLAGS ?= $(CXX_WARNINGFLAGS) $(DEBUGFLAGS) $(FEATUREFLAGS) $(INCLUDES) $(OPTFLAGS) -std=gnu++0x
CC ?= gcc
CXX ?= g++
CC = clang
CXX = clang++
CXXFLAGS += -D__STRICT_ANSI__

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
