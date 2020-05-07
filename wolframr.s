; Wolfram rules player by Gunstick of ULM (c) GPL 2020 and for all eternity...
;the bootsector version has not been tested
;save it from a: to b:
; bug: need to preinit loop and jump in middle...

def_version equ 10
d0_for_mcp equ 0
mcp_adr equ $00000500
bootsector equ 10               ;0=code for boot (uses $60000-$60400)
debug   set  10                  ;0=debug (+ generations in gen:)
withcls set 10     ; 0=no clear screen
ownscreen set 10   ; 0=use screen area in bss instead of default OS
withsound set 10    ; 0=with sound, 10=no sound (needs 14 bytes)
vsync set 10   ; 10=fast, no sync ; 0=vsync  (vsync changes from "digit" to "chiptune")
withmsg set 10 ; 0=with text, 10=no text
supexec set 10 ; 0=Supexec(xbios); 10=Super(gemdos) (2 bytes less)
rndinit set 10 ; 0=start with rule 30 ; 10=use whatever start pattern is in d0.b ; 10=start with 30   (2 bytes less)

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
        addq.w  #6,sp
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
      ; move.b #30-1,d5    ; start with rule 30
     ifeq rndinit
      moveq #30,d5    ; start with rule 30
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

      ifeq withsound
;        move.b #8,$ffff8800.w ; volume A
;        move.b #7,$ffff8802.w ;  max
       ifne vsync
        lea $ffff8800.w,a6
        move.w #$800,(a6)+
        move.b #7,(a6)    ; init PSG with volume max for nasty digit
       endc
      endc
; init screen with 1 pixel set
        movea.l $0000044e.w,a1  ;screenbase
  ; starting pattern
;        move.b #1,32000-80-160(a1)  ; pixel 1 line above so line below is for next generation
        ;move.b #%10001,160*8+80(a1)  ; pixel 1 line above so line below is for next generation
        move.b d5,160*8+86(a1)  ; just use the rule number as starting seed (and notice that it's ignored)
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

; scan all bits of second last line
        move.l (a0),d0 ; get 32 bits at right edge
        moveq #-2,d6   ; skip 2 bits at the start
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
       ifeq vsync
        move.b d7,$ffff8800.w ; psg register 0-9
        move.b d3,$ffff8802.w ; do 'sound'
       endc
        move.b d3,(a6) ; do 'sound'
      endc
        lea 16(a0),a0       ; not 8, as we do 2 columns of 16 pix at once
        dbf d7,_10columns
;  move.w #%11001111,4(a0)
        ; add.l #160,a0  ; next line

        ; vsync
        ifeq vsync
        movem.l d0-d7/a0-a6,-(sp)
        move.w #37,-(sp)
        trap #14
        addq.l #2,sp
        movem.l (sp)+,d0-d7/a0-a6
        endc

      lea 32000(a1),a2
      cmpa.l a2,a0
      bgt.s restart
        cmpi.b  #$39,$fffffc02.w
        bne.s     loop
;        bra.s     loop            ; AND HERE IS THE BIG LOOP


exit:
        rts     ; yeah, save 2 bytes, exit with bombs or something... 
      ifeq withmsg
message:
        dc.b $1b,"b9"   ; text color: 9
        dc.b $1b,"E"    ; CLS
        dc.b "Gunstick's Wolframmer ",$BD," ULM 01.05.2020"  ; 38 
        dc.b 0
      endc
        ifeq bootsector
checksum:
        ds.w  1
b:
        endc

;        endpart
        bss
bss_start:                      ;here starts the bss
;        PART 'bss'
        ds.l    2*40+256
screen:
        ds.l    202*40

;        endpart
bss_end:                        ;here ends the bss
        end

