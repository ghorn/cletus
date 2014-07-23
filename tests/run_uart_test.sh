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



echo "Starting test... "
./test_uart &
echo "... started"

sleep 5

echo "UART port..."
../run_uart &
echo "... activated"



while :
 do
   sleep 1
 done

