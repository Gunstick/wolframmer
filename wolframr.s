; Wolfram rules player by Gunstick of ULM (c) GPL 2020 and for all eternity...
;this is the bootsector version for low rez
;save it from a: to b:
; bug: need to preinit loop and jump in middle...

def_version equ 10
d0_for_mcp equ 0
mcp_adr equ $00000500
bootsector equ 10               ;0=code for boot (uses $60000-$60400)
debug   set  10                  ;0=debug (+ generations in gen:)
megafast set  10                  ;0=faster but not bootsector
withcls set 10     ; 0=no clear screen
ownscreen set 10   ; 0=use screen area in bss instead of default OS
withsound set 10    ; 0=no sound

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
megafast set 1
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
        move.l  #screen,d0   ; here be grafics
        clr.b   d0    ; STf does not know about low byte
        move.l  d0,a0 
        move.w  #0,-(sp) ; mode 0=low
        pea     (a0) ; phys
        pea     (a0) ; log
        move.w  #5,-(sp)  ; setscreen(log, phys, mode)
        trap    #14     ; XBIOS
        lea 12(sp),sp
        endc ; ownscreen

        pea     start(pc)
        move.w  #38,-(sp) ; supexec(start)
        trap    #14 ; XBIOS
        lea 6(sp),sp

        clr.w   -(sp)  ; Pterm0()
        trap    #1  ; GEMDOS
        endc

start:
a:
        pea     message(pc)
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

        move.w  #9,-(sp)  ; Cconws(message)
        trap    #1   ; GEMDOS
        addq.w  #6,sp
        ; set some colors.
        ; background: color 0
        ; left part of columns: color 9
        ; right part of columns: color 1
        ; text color: color 15
        move.l #$00000777,$ffff8240.w   ; pal 0-1   (background,right columns)
        move.l #$07770077,$ffff8250.w   ; pal 8-9   (left columns,ESC-b-9 text)
      ; move.b #30-1,d5    ; start with rule 30
      move.b #99-1,d5    ; start with rule 100
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

      ifeq withsound
        move.b #8,$ffff8800.w ; volume 0
        move.b #7,$ffff8802.w ; 
      endc
; init screen with 1 pixel set
        movea.l $0000044e.w,a1  ;screenbase
  ; starting pattern
;        move.b #1,32000-80-160(a1)  ; pixel 1 line above so line below is for next generation
        move.b #1,160*8+80(a1)  ; pixel 1 line above so line below is for next generation
;        move.b #1,160*8+80+2(a1)  ; pixel 1 line above so line below is for next generation
;        move.b #1,160*8+80+4(a1)  ; pixel 1 line above so line below is for next generation
;        move.b #1,160*8+80+6(a1)  ; pixel 1 line above so line below is for next generation
  ; where we do our business
;        lea 32000-160*2+6(a1),a0  ; second last line, 6 bytes in: first 32bits value
        lea 160*8+6(a1),a0  ; 9th line, 6 bytes in: first 32bits value
    ;    move.w #30,d5   ; we do rule 30
    ;    move.w #%00111100,d5   ; we do mirrored rule 30
   ;     move.w #$ff,d5   ; always on
        addq #1,d5   ; iterate through rules
  ; not.w $ffff8240.w
loop:
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
        moveq #%111,d4
; scan all bits of second last line
        move.l (a0),d0 ; get 32 bits at right edge
        moveq #-3,d6   ; skip 2 bits at the start
        ; we never set the outer bits, is this bad?
        ; get bit pattern of the 3 pixels above
        moveq #9,d7   ; treat 10 columns
_10columns:
        move.l 16(a0),d1 ; get next 32 bits too
        add.w #33,d6  ; treat 32 bits
_32bitloop:
        add.l d1,d1   ; shift left
        addx.l d0,d0  ; get bit in
        addx.b d2,d2  ; accumulate 3 bits
        andi.b #%111,d2   ; mask 3 bits
        ; code idea by Scarab
        btst d2,d5    ; check if matches rule
        sne d4        ; set d1 to ff if true
        add.b d4,d4   ; move d1 value into X
        ;andi #%11101111,ccr  ; clear X
        ;btst.l d2,d5  ; check if matches rule
        ;beq.s notset
        ;ori  #%00010000,ccr  ; set X
notset:
        addx.l d3,d3
        dbf d6,_32bitloop
        move.l d3,160(a0)   ; write line below
      ifeq withsound
        ;move.b #8,$ffff8800.w ; volume 0
        ;move.b #0,$ffff8800.w ; tone 0
        move.b d7,$ffff8800.w ; psg register 0-9
        move.b d3,$ffff8802.w ; do 'sound'
      endc
        lea 16(a0),a0       ; not 8, as we do 2 columns of 16 pix at once
        dbf d7,_10columns
;  move.w #%11001111,4(a0)
        ; add.l #160,a0  ; next line

        ; vsync
        ifeq 0
        movem.l d0-d7/a0-a6,-(sp)
        move.w #37,-(sp)
        trap #14
        addq.l #2,sp
        movem.l (sp)+,d0-d7/a0-a6
        endc
wait:
        cmpi.b  #$80,$fffffc02.w
        cmpi.b  #$1,$fffffc02.w
        blo.s     exit
      lea 32000(a1),a2
      cmpa.l a2,a0
      bgt restart
        bra     loop            ; AND HERE IS THE BIG LOOP


exit:
        rts
message:
        dc.b $1b,"b9"   ; text color: 9
        dc.b $1b,"E"    ; CLS
        dc.b "Gunstick's Wolframmer  ",$BD," ULM 01.05.2020",0,"$"
tables:
        dc.w 0
initpos:
        dc.w 100*160+38+64
initlines:
        dc.w 1
;        dc.w 45
checksum:
        ds.w  1
b:


;        endpart
        bss
bss_start:                      ;here starts the bss
;        PART 'bss'
        ds.w 4
emptylines1:
        ds.w 410                ;init to 1
emptylines2:
        ds.w 410                ;needs no init
        ds.w 4
lastline1:
        ds.w 40+4            ;needs no init
lastline2:
        ds.w 40+4            ;needs no init
        ds.l    2*40+256
screen:
        ds.l    202*40

;        endpart
bss_end:                        ;here ends the bss
        end

