#!/bin/bash
# Run script 
trap 'my_exit; exit' SIGINT SIGQUIT
count=0

my_exit()
{
echo "you hit Ctrl-C/Ctrl-\, now exiting.."
echo "...sensors"
killall -9 run_sensors &
echo "...contoller"
killall -9 run_controller &
echo "...actuators"
killall -9 run_actuators &
exit 1
}


echo "Ready for take off?"
read -p "Press any key to continue... " -n1 -s

echo "Starting run_actuators... "
./run_actuators 49 100 &
echo "... started"

sleep 3

echo "Starting run_controller..."
./run_controller 48 100 &
echo "... started"

sleep 3

echo "Starting run_sensors..."
./run_sensors 47 100&
echo "... started"




while :
 do
   sleep 1
 done

