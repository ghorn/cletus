// Copyright 2014, University of Freiburg
// Systems Theory Lab
// Author: Elias Rosch <eliasrosch@googlemail.com>

#include "./UDP.h"

/*
 * Class for using UDP as transmission protocol
 * for the MPU 9xxx sensor values
 */

UDP::UDP(){
}

UDP::~UDP(){
}

// Initializes the communication via udp
void UDP::initUDP() {
    //create a UDP socket
    if ((_udp_socket=socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) == -1) {
        printf("Failed to create own udp socket: %s\n", strerror(errno));
        exit(1);
    }
    
    // Set the contents of the personal address struct
    memset((char *) &_addr_me, 0, sizeof(_addr_me));
    _addr_me.sin_family = AF_INET;
    _addr_me.sin_port = htons(PORT);
    _addr_me.sin_addr.s_addr = htonl(INADDR_ANY); // Receives messages from any IP-Address
    // inet_aton("63.161.169.137", &_addr_me.sin_addr.s_addr); // Receives only messages from specified IP-Address

    // bind socket to port
    if( bind(_udp_socket, (struct sockaddr*)&_addr_me, sizeof(_addr_me) ) == -1) {
    printf("Failed to bind socket to port: %s\n", strerror(errno));
    exit(1);
    }
<<<<<<< HEAD

    //Set the contents of the other address struct
    memset((char *) &_addr_other, 0, sizeof(_addr_other));
    _addr_other.sin_family = AF_INET;
    _addr_other.sin_port = htons(PORT);
    if (inet_aton(SERVER_IP, &_addr_other.sin_addr) == 0) {
        printf("Failed to create other udp socket: %s\n", strerror(errno));
        exit(1);
    }
    _socketlen = sizeof(_addr_other);
    printf("Socket initialized\n");
}

int16_t UDP::receiveUDP() {
    char buf[MAXBUF] = {0};
    // try to receive some data ( blocking! )
    printf("Trying to receive message...");
    fflush(stdout);

    if ((_recv_len = recvfrom(_udp_socket, buf, MAXBUF, 0, (struct sockaddr *)&_addr_other, (socklen_t*)&_socketlen)) == -1) {
        printf("Failed to receive udp data: %s\n", strerror(errno));
        exit(1);
     }
     //if (inet_ntoa(_addr_other.sin_addr) == SENDER_IP) {
     //printf("Received packet: %i", atoi(buf));
     //} else {
     //   printf("Received from wrong IP-Address: %s\n", inet_ntoa(_addr_other.sin_addr));
     //}
     return (int16_t) atoi(buf);
}

void UDP::sendUDP(int16_t value) {
    char buf[MAXBUF];
    sprintf(buf, "%i", value);
    printf("Socket: %i, Length: %i, Socketlength: %i \n", _udp_socket, strlen(buf), _socketlen);
    if (sendto(_udp_socket, buf, strlen(buf), 0, (struct sockaddr*)&_addr_other, _socketlen) == -1) {
        printf("Failed to send udp data: %s\n", strerror(errno));
        exit(1);
    }
}

/*
void UDP::closeUDP() {
    close(_udp_socket);
}
*/
