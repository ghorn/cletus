// Copyright 2014, University of Freiburg
// Systemtheory Lab
// Author: Elias Rosch <eliasrosch@googlemail.com>

/*
 * Simple udp server
 */ 
    
#include<stdio.h>
#include<string.h>
#include<stdlib.h>
#include<errno.h>
#include<arpa/inet.h>
#include<sys/socket.h>
 
#define MAXBUF 512  //Max length of buffer
#define PORT 8888   //The port on which to listen for incoming data
 
int main(void)
{
    // This structs handle the adresses
    struct sockaddr_in addr_me, addr_other;
     
    int udp_socket, i, socketlen = sizeof(addr_other) , recv_len;
    char buf[MAXBUF];
     
    //create a UDP socket
    if ((udp_socket=socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) == -1) {
        printf("Failed to create udp socket: %s\n", strerror(errno));
        exit(1);
    }
     
    // Set the contents of the address struct
    memset((char *) &addr_me, 0, sizeof(addr_me));
    addr_me.sin_family = AF_INET;
    addr_me.sin_port = htons(PORT);
    addr_me.sin_addr.s_addr = htonl(INADDR_ANY);
    // inet_aton("63.161.169.137", &addr_me.sin_addr.s_addr);
     
    // bind socket to port
    if( bind(udp_socket, (struct sockaddr*)&addr_me, sizeof(addr_me) ) == -1) {
        printf("Failed to bind socket to port: %s\n", strerror(errno));
        exit(1);
    }
     
    // listen for data
    while(1)
    {
        printf("Waiting for data...");
        fflush(stdout);
         
        // Erase the content of the buffer
        memset(buf,'\0', MAXBUF);
        // try to receive some data ( blocking! )
        if ((recv_len = recvfrom(udp_socket, buf, MAXBUF, 0, (struct sockaddr *)&addr_other, &socketlen)) == -1)
        {
            printf("Failed to receive udp data: %s\n", strerror(errno));
            exit(1);
        }
         
        //print details of the client/peer and the data received
        printf("Received packet from %s:%d\n", inet_ntoa(addr_other.sin_addr), ntohs(addr_other.sin_port));
        printf("Data: %s\n" , buf);
         
        //now reply the client with the same data
        if (sendto(udp_socket, buf, recv_len, 0, (struct sockaddr*)&addr_other, socketlen) == -1)
        {
            printf("Failed to send udp data: %s\n", strerror(errno));
            exit(1);
        }
    }
 
    close(udp_socket);
    return 0;
}
