// Copyright 2014, University of Freiburg
// Systems Theory Lab
// Author: Elias Rosch <eliasrosch@googlemail.com>

#ifndef CPP_UDP_UDP_H
#define CPP_UDP_UDP_H

#include<stdio.h>
#include<string.h>
#include<stdlib.h>
#include<errno.h>
#include<sys/socket.h>
#include<arpa/inet.h>

#define MAXBUF 512
#define PORT 8080
#define SERVER_IP "10.42.0.1"

/*
 * Class for using UDP as transmission protocol
 * for the MPU 9xxx sensor values
 */

class UDP {
    public:
        // Constructor/Destructor
        UDP();
        ~UDP();

        // Functions to initialize the UDP communication
        void initUDP();
        
        // Send 16-Bit-integer via UDP
        void sendUDP(int16_t value);

        // Receive 16-Bit integer via UDP
        int16_t receiveUDP();

        // Close the udp-socket
        //void closeUDP();

    private:
        struct sockaddr_in _addr_me;
        struct sockaddr_in _addr_other;
        int _udp_socket;
        int _socketlen;
        int _recv_len;
};

#endif // CPP_UDP_UDP_H
