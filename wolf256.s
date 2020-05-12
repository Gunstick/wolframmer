; Wolfram rules player by Gunstick of ULM (c) GPL 2020 and for all eternity...
;the bootsector version has not been tested
;save it from a: to b:
; bug: need to preinit loop and jump in middle...

  opt  O+ ; all optimizations on 

def_version equ 10
bootsector equ 10               ;0=code for boot (uses $60000-$60400)
debug   set  10                  ;0=debug (+ generations in gen:)
withcls set 10     ; 0=no clear screen
ownscreen set 10   ; 0=use screen area in bss instead of default OS
withsound set 0    ; 0=with sound, 10=no sound (needs 48 bytes)
withcolors set 0    ; 0=with colors, 10=no colors (needs 14 bytes)
vsync set 10   ; 10=fast, no sync ; 0=vsync  (vsync changes from "digit" to "chiptune")
withmsg set 0 ; 0=with text, 10=no text
supexec set 10 ; 0=Supexec(xbios); 10=Super(gemdos) (2 bytes less)
rndinit set 00 ; 0=start with rule 30 ; 10=use whatever start pattern is in d5.b (2 bytes less)
gray set 10 ; 0 = play gray codes ; 10 = play binary (gray = 14 bytes more)
iteration set 1 ; -1 = reverse ; 0 = no rule change ; 1=incremantal  (same size)

; principle:
; Screen is organized in 10 vertical stripes of 32 pixels
; always meging plane4 of first colums with plane1 of second column
;             0      31                  ... 32      63
; aaaabbbbccccDDDDAAAAbbbbccccddddaaaabbbbccccDDDDAAAAbbbbccccdddd
; | 1st 16 pixels|| 2nd 16 pixels|| 3rd 16 pixels|| 4th 16 pixels|
;            $DDDDAAAA is the first 32bit value


; some helper routine to make a boot sector
; Installe boot secteur
        ifeq bootsector
debug   set 1
        opt     x+

        move.l  #b,d0
        sub.l   #a,d0
        cmp.w   #512,d0
        bgt     Fin
        
        move.w  #1,-(sp)
        move.w  #0,-(sp)
        move.w  #0,-(sp)
        move.w  #1,-(sp)
        move.w  #0,-(sp)
        clr.l   -(sp)
        move.l  #buffer,-(sp)
        move.w  #8,-(sp)
        trap    #14
        lea     20(sp),sp
        
        
        lea     a,a0
        lea     cpy,a2
        lea     buffer-(cpy-a),a1
        
        moveq   #cpy-a-1,d0
Bcl     move.b  (a1)+,(a2)+
        dbf     d0,Bcl

        clr.w   510(a0)
        lea     a,a1
        move.w  #$ff,d7
        moveq   #0,d0
calcchecksum
        add.w   (a1)+,d0
        dbf     d7,calcchecksum
        move.w  #$1234,d1
        sub.w   d0,d1
        move.w  d1,510(a0)

        move.w  #1,-(sp)
        move.w  #0,-(sp)        
        move.w  #0,-(sp)        
        move.w  #1,-(sp)        
        move.w  #0,-(sp)        
        clr.l   -(sp)
        move.l  #a,-(sp)
        move.w  #9,-(sp)
        trap    #14
        lea     20(sp),sp
; could add here actual code to write the bootsector with a prompt
; really!       
Fin     clr.w   -(sp)
        trap    #1
        
        section BSS
buffer  ds.b    512

        endc    ; bootsector generator
;        >PART 'my sync'

;        default 1

        opt X-,D-

        text
x:
        ifne def_version

        ifeq ownscreen
    ; this could all go away if we just draw on the current screen!
        lea screen(pc),a0
        move.l  a0,d0   ; here be grafics
        clr.b   d0    ; STf does not know about low byte
        move.l  d0,a0 
        move.w  #0,-(sp) ; mode 0=low
        pea     (a0) ; phys
        pea     (a0) ; log
        move.w  #5,-(sp)  ; setscreen(log, phys, mode)
        trap    #14     ; XBIOS
        lea 12(sp),sp
        endc ; ownscreen

     ifeq supexec
        pea     start(pc)
        move.w  #38,-(sp) ; supexec(start)
        trap    #14 ; XBIOS
;        lea 6(sp),sp
;        clr.w   -(sp)  ; Pterm0()
;        trap    #1  ; GEMDOS
     else
        clr.l   -(sp)
        move.w   #32,-(sp)  ; super
        trap   #1    ; GEMDOS
;        addq.l   #6,sp   ; needed?  
     endc

        endc

start:
a:
      ifeq withmsg
        pea     message(pc)
      endc
      ifeq bootsector
        bra.s   bootcode
        dc.b "GK"
gen:
cpy:
        dc.b 12,34,56         ;serial
        ds.b 1
bitpatt:
        ds.b 8
topline:
        ds.b 8
bootcode:
      endc   ; bootsector
      ifeq withmsg
        move.w  #9,-(sp)  ; Cconws(message)
        trap    #1   ; GEMDOS
     ;   addq.w  #6,sp      ; who cares?
      endc
        ; set some colors.
        ; background: color 0
        ; left part of columns: color 9
        ; right part of columns: color 1
        ; text color: color 15
     ifeq withmsg
        move.l #$00000777,$ffff8240.w   ; pal 0-1   (background,right columns)    #8
        move.l #$07770077,$ffff8250.w   ; pal 8-9   (left columns,ESC-b-9 text)   #8
     else
         move.w $ffff8250.w,$ffff8242.w   ; pal 8->1   (left=right columns)    #6
;        move.w #$0777,$ffff8250.w   ; pal 8   (left columns)   #6
     endc

     ifeq rndinit
      ifeq gray
        moveq #20-iteration,d5    ; start with rule 30 (30 is the value of the 20th gray code)
        swap d5
      else
        moveq #30-iteration,d5
      endc
     endc
      ifeq withsound
       ifne vsync
        lea $ffff8800.w,a6
	lea 2(a6),a5
	lea soundinit(pc),a4
        moveq #13,d0
.i:
        move.b d0,(a6)
        move.b (a4)+,(a5)
        dbf d0,.i
       endc
      endc
restart:

        ifeq withcls
        movea.l $0000044e.w,a1  ;screenbase
;        lea     -160-4(a1),a1
        move.w  #21*200-1-10,d0
cls:
        move.w  #$ffff,(a1)+
        clr.l   (a1)+
        addq.w  #2,a1
        dbra    d0,cls
        endc ; withcls

; init screen with 1 pixel set
        movea.l $0000044e.w,a1  ;screenbase
;        lea 32000-160*2+6(a1),a0  ; second last line, 6 bytes in: first 32bits value
        lea 160*8+6(a1),a0  ; 9th line, 6 bytes in: first 32bits value
    ;    move.w #30,d5   ; we do rule 30
    ;    move.w #%00111100,d5   ; we do mirrored rule 30
   ;     move.w #$ff,d5   ; always on
	ifeq gray
	  ; how to gray code?
	  ; gray = i eor (i>>1)

	  swap d5   ; get counter  ; #2    (instead of storing counter on low memory, using high word of d5 is 2 bytes smaller)
	  ifne iteration
	    addq.w #iteration,d5   ; #2 iterate through
	  endc
	  move.w d5,d4  ; copy counter ; #2
	  swap d5    ; store counter ; #2
	  move.w d4,d5   ; #2
	  lsr.w #1,d4
	  eor.w d4,d5
	else
	  ifne iteration
	    addq.w #iteration,d5   ; #2 iterate through
	  endc	
	endc   ; gray
  ; not.w $ffff8240.w
  ; starting pattern
;        move.b #1,32000-80-160(a1)  ; pixel 1 line above so line below is for next generation
        ;move.b #%10001,160*8+80(a1)  ; pixel 1 line above so line below is for next generation
        move.l d5,160*8+16*4+6(a1)  ; just use the rule number as starting seed (and notice that it's ignored)
;        bset.w #0,160*8+16*5+6(a1)  ; pixel aorund middle of screen
;        bset.w #0,160*8+16*1+6(a1)  ; pixel 1 line above so line below is for next generation
;        bset.w #0,160*8+6(a1)  ; pixel 1 line above so line below is for next generation
;        move.b #1,160*8+80+4(a1)  ; pixel 1 line above so line below is for next generation
;        move.b #1,160*8+80+6(a1)  ; pixel 1 line above so line below is for next generation
  ; where we do our business
loop:
	
      lea 32000(a1),a2
      cmpa.l a2,a0
      bgt.s restart   
        ifeq debug
        addq.w  #1,gen
        endc
; d0=current value
; d1=value to the right
; d2=shifted out bits (masked to 3)
; d3=calculated output
; d4=%111 (mask for d2)
; d4=temp register
; d5=rule number, i.e. 30 ($1E, %00011110)
; d6=loop counter per column (32)
; d7=loop counter for line (10)
; a0=beginning of line+6, so first 32bit value is at 0(a0), next at 8(a0) until 72(a0)
; a1=pyhsbase
; let's first implement an "any rule" algorithm

columns set 11    ; how many columns to do  (+1 because reasons)
codeversion set 0 ; 0 = NEWCODE
; scan all bits of second last line
        move.l (a0),d0 ; get 32 bits at right edge
	moveq.l #0,d3
  ifeq codeversion  ; NEWCODE
        moveq #0,d6   ; fall through 1st loop 
        moveq #(columns-1),d7   ; treat 10 columns
	lea bof(pc),a4
;	move.l (a4),a3   ; save memory as 1st write deletes it
	bra.s initload
_10columns:
	moveq #30,d6
_31bitloop:
        add.l d1,d1   ; shift left
        addx.l d0,d0  ; get bit in
        addx.b d2,d2  ; accumulate 3 bits
        andi.b #%111,d2   ; mask 3 bits
        ; code idea by Scarab
        btst d2,d5    ; check if matches rule
        sne d4        ; set d4 to ff if true
        add.b d4,d4   ; move d1 value into X
        addx.l d3,d3
	dbf d6,_31bitloop 
	; do last bit (from new read), then wite and loop
initload:
        move.l 16(a0),d1 ; get next 32 bits too
        add.l d1,d1   ; shift left
        addx.l d0,d0  ; get bit in
        addx.b d2,d2  ; accumulate 3 bits
        andi.b #%111,d2   ; mask 3 bits
        ; code idea by Scarab
        btst d2,d5    ; check if matches rule
        sne d4        ; set d4 to ff if true
        add.b d4,d4   ; move d1 value into X
        addx.l d3,d3
	move.l d3,(a4)   ; 1st write goes nowhere
	lea 160(a0),a4   ; next write goes here
  endc ; NEWCODE
  ifne codeversion  ; OLDCODE
;        moveq #-2,d6   ; skip 2 bits at the start
;        moveq #-1,d6   ; skip 1 bits at the start
        moveq #0,d6   ; no skip
        ; we never set the outer bits, is this bad?
        ; get bit pattern of the 3 pixels above
        moveq #(columns-1),d7   ; treat 10 columns
_10columns:
        move.l 16(a0),d1 ; get next 32 bits too
        add.w #32,d6  ; treat 32 bits
_32bitloop:
        add.l d1,d1   ; shift left
        addx.l d0,d0  ; get bit in
        addx.b d2,d2  ; accumulate 3 bits
        andi.b #%111,d2   ; mask 3 bits
        ; code idea by Scarab
        btst d2,d5    ; check if matches rule
        sne d4        ; set d4 to ff if true
        add.b d4,d4   ; move d1 value into X
        addx.l d3,d3
        dbf d6,_32bitloop
        move.l d3,160(a0)   ; write line below
  endc  ; OLDCODE

        ifeq withsound
        ;move.b #8,$ffff8800.w ; volume 0
        ;move.b #0,$ffff8800.w ; tone 0
          ifeq vsync
;        move.b d7,$ffff8800.w ; psg register 0-9
;        move.b d3,$ffff8802.w ; do 'sound'
          endc
;        move.b d3,(a6) ; do 'sound'
          move.b #10,(a6) ; do 'sound'
          move.b d3,(a5) ; do 'sound'
          move.b #3,(a6)
          move.l d5,(a5)  ;goes from non-pleasing sound to annoying after 256 steps
        endc
        ifeq withcolors
          move.w d5,$ffff8240.w  
          move.w d3,$ffff8242.w
          add.w d3,d3
          move.w d3,$ffff8250.w
        endc
        lea 16(a0),a0       ; not 8, as we do 2 columns of 16 pix at once
        dbf d7,_10columns
        
        lea 160-(columns*16)(a0),a0  ; next line
;	lea -(columns*16)(a4),a0   ; this makes interesting forest effects.
        ; vsync
        ifeq vsync
        movem.l d0-d7/a0-a6,-(sp)
        move.w #37,-(sp)
        trap #14
        addq.l #2,sp
        movem.l (sp)+,d0-d7/a0-a6
        endc

        cmpi.b  #$39,$fffffc02.w
        bne     loop
;        bra.s     loop            ; AND HERE IS THE BIG LOOP


exit:
        rts     ; yeah, save 2 bytes, exit with bombs or something... 
      ifeq withmsg
message:
        dc.b $1b,"b9"   ; text color: 9
        dc.b $1b,"E"    ; CLS
        dc.b $1b,"f"    ; cursor off
message2:
        dc.b $1b,"H","Gunstick's Wolframmer ",$BD," ULM 12.05.2020"  ; 38 
        dc.b 0
      endc
        ifeq bootsector
checksum:
        ds.w  1
b:
        endc
	ifeq withsound
soundinit:
	;     13,   12, 11, 10,   9,  8  , 7
	;     E     freqE   Vc   Vb   Va   Mix
	dc.b  %1010, 0, $2e, $f, $10, $10, %111100
	;      6,  5,  4,  3,  2,  1,  0
	;     N   freqC  freqB   freqA
	dc.b   0,  0,  0,$ee, $e,$ef, $e 
	endc
;        endpart
        bss
; hack some low memory "registers" into the OEM region $200-$37F
generation set $200    ; generation counter
bof:
	dc.l 0
bss_start:                      ;here starts the bss
;        PART 'bss'
        ds.l    2*40+256
screen:
        ds.l    202*40

;        endpart
bss_end:                        ;here ends the bss
        end

