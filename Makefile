#!/usr/bin/make -f
#


.s:
	vasmm68k_mot -Ftos -align -devpac -m68000 -showopt -nosym -o $@.prg $<
.o:
	vlink $<

