# What's the executable called?
PROJ = run_sensors run_controller run_actuators

# What C or C++ files must we compile to make the executable?
SRC ?= run_sensors.c \
       run_controller.c \
       run_actuators.c \
       controller.c \
       sensors.c \
       misc.c \
       zmq.c

# What directories must we include?
INCLUDENAMES ?= # e.g. ../mathlib; the makefile will add the -I

OTHERINCLUDE ?=

# With what libraries should we link?
LIBNAMES ?= zmq m # e.g. m gsl atlas; the makefile will add the -l
LIBDIRS ?= # e.g. ../; the makefile will add the -L
OTHERLIB ?=

# You can add custom compiler and linker flags here.  USERFLAGS gets
# used by all compiler and linker calls, except when creating a static
# lib.  The others are specific to their stage.
USERFLAGS ?=
ifdef DEBUG
USERFLAGS += -DDEBUG
endif
ifdef SPAM
USERFLAGS += -DSPAM
endif
USERCFLAGS ?=
USERCXXFLAGS ?=
USERLDFLAGS ?=

ifdef DEVNAME
USERFLAGS += -DDEVNAME=\"$(DEVNAME)\"
endif

MKFILE_DIR = make/

# Use the Google C++ linter on C files, rather than the 'splint' command
C_LINT = cpplint.py

# Use the Google C++ linter for flymake targets.
EXTRA_CHECKS = lint

include $(MKFILE_DIR)common_head.mk
include $(MKFILE_DIR)native.mk
include $(MKFILE_DIR)build.mk
include $(MKFILE_DIR)test.mk
include $(MKFILE_DIR)syntax.mk
include $(MKFILE_DIR)clean.mk
include $(MKFILE_DIR)common_tail.mk

docs:
	@echo Generating docs \(requires the 'redcarpet' ruby gem\).
	$(Q)redcarpet README.md > README.html && x-www-browser README.html
