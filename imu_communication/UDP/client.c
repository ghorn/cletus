// Copyright 2014, University of Freiburg
// Systemtheory Lab
// Author: Elias Rosch <eliasrosch@googlemail.com>

/*
 * Simple udp client
 */

#include<stdio.h>
#include<string.h>
#include<stdlib.h>
#include<errno.h>
#include<arpa/inet.h>
#include<sys/socket.h>
 
#define SERVER_IP "10.42.0.1"
#define MAXBUF 512  //Max length of buffer
#define PORT 8888   //The port on which to send data
 
int main(void) {
    struct sockaddr_in addr_other;
    int udp_socket, i, socketlen=sizeof(addr_other);
    char buf[MAXBUF];
    char message[MAXBUF];
 
    if ( (udp_socket=socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) == -1) {
        printf("Failed to create udp socket: %s\n", strerror(errno));
        exit(1);
    }

    // Set the contents of the address struct
    memset((char *) &addr_other, 0, sizeof(addr_other));
    addr_other.sin_family = AF_INET;
    addr_other.sin_port = htons(PORT);
     
    if (inet_aton(SERVER_IP , &addr_other.sin_addr) == 0) {
        printf("Failed to set up host-ip-address: %s\n", strerror(errno));
        exit(1);
    }
 
    while(1) {
        printf("Enter message : ");
        gets(message);
         
        //send the message
        if (sendto(udp_socket, message, strlen(message) , 0 , (struct sockaddr *) &addr_other, socketlen)==-1) {
            printf("Failed to send message to host: %s\n", strerror(errno));
            exit(1);
        }
         
        //receive a reply and print it
        //clear the buffer by filling null, it might have previously received data
        memset(buf,'\0', MAXBUF);
        //try to receive some data, this is a blocking call
        if (recvfrom(udp_socket, buf, MAXBUF, 0, (struct sockaddr *) &addr_other, &socketlen) == -1) {
            printf("Failed to receive udp-data: %s\n", strerror(errno));
            exit(1);
        } 
        puts(buf);
    } 
    close(udp_socket);
    return 0;
}
