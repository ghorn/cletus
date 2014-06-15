#!/bin/bash
# Run script 
trap 'my_exit; exit' SIGINT SIGQUIT
count=0

my_exit()
{
echo "you hit Ctrl-C/Ctrl-\, now exiting.."
echo "...sensors"
pkill run_sensors
echo "...contoller"
pkill run_controller
echo "...actuators"
pkill run_actuators
}

echo "Starting run_sensors... \n"
./run_sensors &
echo "... started \n \n"

echo "Starting run_controller... \n"
./run_controller &
echo "... started \n \n"

echo "Starting run_actuators... \n"
./run_controller &
echo "... started \n \n"

while :
 do
   sleep 1
 done

