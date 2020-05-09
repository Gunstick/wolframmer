#!/bin/bash
if [ "$1" = "" ]
then
  echo "usage: $0 filename"
  exit 1
fi
filebase="${1%.*}"
while true
do
  if [ "${filebase}.s" -nt "${filebase}.tos" ]
  then
    killall -9 hatari;
    make ${filebase} 
    if [ $? -eq 0 ]
    then
      ls -l ${filebase}.tos
      hatari ${filebase}.tos &
    fi
    sleep 1
    WID=$(xdotool search --onlyvisible --name hatari)
    xdotool windowmove $WID 0 0
  fi;
  sleep 1 ;
done
