#!/bin/bash
while true
do
  if [ "wolframr.s" -nt "wolframr.tos" ]
  then
    killall -9 hatari;
    make wolframr 
    if [ $? -eq 0 ]
    then
      ls -l wolframr.tos
      hatari wolframr.tos &
    fi
    sleep 1
    WID=$(xdotool search --onlyvisible --name hatari)
    xdotool windowmove $WID 0 0
  fi;
  sleep 1 ;
done
