# I am a comment, and I want to say that the variable CC will be
# the compiler to use.
CC=gcc
# Hey!, I am comment number 2. I want to say that CFLAGS will be the
# options I'll pass to the compiler.
CFLAGS=-c -Wall -std=c99 -Werror -DDEBUG=1 -D _BSD_SOURCE -O3
INCLUDES= -I. -I/usr/include -I.. -pthread 

all: hello

hello: main_lisa_communication.o circular_buffer.o data_decoding.o log.o uart_communication.o
	$(CC) $(INCLUDES) main_lisa_communication.o circular_buffer.o data_decoding.o log.o uart_communication.o -o main_lisa_communication

main_lisa_communication.o: main_lisa_communication.c
	$(CC) $(CFLAGS) $(INCLUDES) main_lisa_communication.c

circular_buffer.o: circular_buffer.c
	$(CC) $(CFLAGS) $(INCLUDES) circular_buffer.c

data_decoding.o: data_decoding.c
	$(CC) $(CFLAGS) $(INCLUDES) data_decoding.c

uart_communication.o: uart_communication.c
	$(CC) $(CFLAGS) $(INCLUDES) uart_communication.c

log.o: log.c
	$(CC) $(CFLAGS) $(INCLUDES) log.c

clean:
	rm -rf *o hello
