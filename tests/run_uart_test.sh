#!/bin/bash
# Run script 
trap 'my_exit; exit' SIGINT SIGQUIT
count=0

my_exit()
{
echo "you hit Ctrl-C/Ctrl-\, now exiting.."
echo "...uart"
killall -9 run_uart &
echo "...test"
killall -9 test_uart &
}



echo "Starting run_uart... "
../run_uart &
echo "... started"

sleep 3

echo "Starting test..."
./test_uart &
echo "... started"



while :
 do
   sleep 1
 done

