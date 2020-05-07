#!/bin/bash
# shortens a relocatable TOS prog, ttp ot tos by 4 bytes by setting the reloc flag and removing reloc table
# get reloc flag
f="$(od -x --skip-bytes=26 --read-bytes=2 --endian=big "$1"  | awk '{print $2;exit}')" 
echo "Flag=$f"
if [ "$f" = "0000" ]
then
  # need to change
  (size=$(ls -l "$1"|awk '{print $5}')
  dd if="$1" bs=1 count=26
  echo ; echo   # silly flag
  dd if="$1" bs=1 skip=28 count=$((size-28-4)) ) >  "$1".new
  cat "$1".new > "$1"
  rm "$1".new
else
  echo "Nothing to do"
fi
