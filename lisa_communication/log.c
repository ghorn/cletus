#include <stdio.h>
#include <time.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include "log.h"

#ifndef DEBUG 
#define DEBUG 0
#endif

// This code is for logging data on the plane bbone

LOG_errCode mount_sd_card();
 
FILE *lisa_log_file,*groundstation_log_file,*boneplane_log_file;

const char FILE_PATH_LISA_LOG[] = "/home/ubuntu/log/data_lisa_log.txt";
const char FILE_PATH_GROUND_LOG[] = "/home/ubuntu/log/data_groundstation_log.txt";
const char FILE_PATH_BONEPLANE_LOG[] = "/home/ubuntu/log/data_boneplane_log.txt";

const char FILE_PATH_PROGRAM_LOG[]="log/log.txt";
const char FILE_PATH_PROGRAM_ERROR[]="log/error.txt";

const char LOG_FOLDER_LOCATION[] = "/home/ubuntu/log";
/********************************
 * FUNCTIONS
 * ******************************/
 
LOG_errCode init_log(){
	#if DEBUG  > 1
		printf("Entering init_log\n");
	#endif
   //check if sdcard is present
	FILE *file;
    file = fopen("/home/ubuntu/log/log_folder_present","w");
    if(file==NULL){
        char command[50]= "mkdir ";
        strcat(command, LOG_FOLDER_LOCATION);
        if(system(command) != 0){
            return LOG_ERR_FOLDER;
        }
    }
	return LOG_ERR_NONE;
}



/**********************************
LOG FOR DATA COMING FROM LISA
***********************************/
LOG_errCode open_data_lisa_log(){
	#if DEBUG  > 1
		printf("Entering open_data_lisa_log\n");
	#endif
	
	lisa_log_file = fopen(FILE_PATH_LISA_LOG,"a+"); 
	if(lisa_log_file==NULL){
			return LOG_ERR_OPEN_FILE;
	}
	return LOG_ERR_NONE;
}

LOG_errCode write_data_lisa_log(char *data,int length){
	#if DEBUG  > 1
		printf("Entering write_data_lisa_log\n");
	#endif
	
	if(fwrite(data,length, 1, lisa_log_file)<1){
		return LOG_ERR_WRITE; 
	}
	return LOG_ERR_NONE;
}

LOG_errCode close_data_lisa_log(){
	#if DEBUG  > 1
		printf("Entering close_data_lisa_log\n");
	#endif
	
	if(fclose(lisa_log_file)==EOF){
		return LOG_ERR_CLOSE; 
	}
	return LOG_ERR_NONE;
}

/**********************************
LOG FOR DATA COMING FROM GROUNDSTATION
***********************************/
LOG_errCode open_data_groundstation_log(){
	#if DEBUG  > 1
		printf("Entering open_data_groundstation_log\n");
	#endif
	
	groundstation_log_file = fopen(FILE_PATH_GROUND_LOG,"a+"); 
	if(groundstation_log_file==NULL){
			return LOG_ERR_OPEN_FILE;
	}
	return LOG_ERR_NONE;
}

LOG_errCode write_data_groundstation_log(char *data,int length){
	#if DEBUG  > 1
		printf("Entering write_data_groundstation_log\n");
	#endif
	
	if( fwrite(data, length, 1, groundstation_log_file)!=length ){
		return LOG_ERR_WRITE; 
	}
	
	return LOG_ERR_NONE;
}

LOG_errCode close_data_groundstation_log(){
	#if DEBUG  > 1
		printf("Entering close_data_groundstation_log\n");
	#endif
	
	if(fclose(groundstation_log_file)==EOF){
		return LOG_ERR_CLOSE; 
	}
	return LOG_ERR_NONE;	
}


/**********************************
LOG FOR ERRORS FROM PROGRAM 
***********************************/
LOG_errCode error_write(char *file_name,char *message){
	#if DEBUG  > 1
		printf("Entering error_write\n");
	#endif
	
	FILE *file; 
	time_t now = time(0);
	char* time_string;
	time_string = ctime(&now);
    file = fopen(FILE_PATH_PROGRAM_ERROR,"a+"); 
    
    if(file==NULL){
		return LOG_ERR_OPEN_FILE;
	}
     
	if(fprintf(file,"%s%s\t%s\n\n",time_string,file_name,message)<0){
		return LOG_ERR_WRITE; 
	}
	
	if(fclose(file)==EOF){
		return LOG_ERR_CLOSE; 
	}
	
	return LOG_ERR_NONE;
}

LOG_errCode log_write(char *file_name,char *message){
	#if DEBUG  > 1
		printf("Entering log_write\n");
	#endif
	
	//write error to log		
	FILE *file; 
	time_t now = time(0);
	char* time_string;
	time_string = ctime(&now);
    file = fopen(FILE_PATH_PROGRAM_LOG,"a+"); 
    
     if(file==NULL){
		return LOG_ERR_OPEN_FILE;
	}
    
	if(fprintf(file,"%s%s\t%s\n\n",time_string,file_name,message)<0){
		return LOG_ERR_WRITE; 
	}
	
	if(fclose(file)==EOF){
		return LOG_ERR_CLOSE; 
	}	
	return LOG_ERR_NONE;
}

void LOG_err_handler(LOG_errCode err,void (*write_error_ptr)(char *,char *,int))  
{
	#if DEBUG  > 1
		printf("Entering LOG_err_handler\n");
	#endif
	
	static char SOURCEFILE[] = "log.c";
	//write error to local log
	switch( err ) {
		case LOG_ERR_NONE:
			break;
		case  LOG_ERR_UNDEFINED:
			write_error_ptr(SOURCEFILE,"undefined log error",err);
			break;
        case LOG_ERR_FOLDER:
			write_error_ptr(SOURCEFILE,"error while mounting sd card",err);
			break;
		case LOG_ERR_OPEN_FILE:
			write_error_ptr(SOURCEFILE,"error opening file",err);
			break;
		case LOG_ERR_WRITE:
			write_error_ptr(SOURCEFILE,"error writing to file",err);
			break;
		case LOG_ERR_CLOSE:
			write_error_ptr(SOURCEFILE,"error closing file",err);
			break;
		default: break;
	}
}
