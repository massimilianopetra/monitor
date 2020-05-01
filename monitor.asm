;
;         PPS-2 Monitor
; Copyright Massimiliano Petra April, 2020
; massimiliano.petra@gmail.com
;
;

; Constant
RETURN  equ $8D         ; Return key code

; Page Zero Var
SCRADDR equ $28         ; Indirect screen base address for echo (low address)

; Memory mappef Var
KBRD    equ $C000       ; I/O Input keyboard
STROBE  equ $C010       ; I/O Strobe keyboard
PROMPT  equ $200        ; Prompt char
JMPADR  equ $202        ; Jump address
BUFFER  equ $280        ; Line buffer in
VAR1    equ $300        ; Temp variable 1
VAR2    equ $300        ; Temp variable 2
VAR3    equ $300        ; Temp variable 3



; ***************************************************
; MACRO FUNCTIONS
; ***************************************************

echo    macro
        sta (SCRADDR),y
        endm


print   macro pos,string
        ldx #0
        ldy #pos
loop\?  lda string,x
        beq last\?
        ora #$80
        sta (SCRADDR),y
        inx
        iny 
        jmp loop\?
last\?
        endm

vout    macro l,pos,string
        ldx #0
        ldy #0
loop\?  lda string,x
        beq last\?
        ora #$80
        sta 1024+256*((l/2) % 4)+(128*(l % 2))+40*((l/8) % 4)+pos,y
        inx
        iny 
        jmp loop\?
last\?
        endm

readline macro pos
        lda #pos
        jsr RDLINE
        endm

input   macro
        jsr INPUT
        endm
        
clearline macro pos
        lda #pos
        jsr CLRLIN
        endm
        
scroll  macro
        jsr SCROLL
        endm

clrscr  macro
        jsr CLRSCR
        endm
        
; Moniotr start
        org $F000

START   
        cld                     ; Disable BCD mode
        cli                     ; Enable Interrupt
        clrscr                  ; Clear Screen    
        vout 0,16,_SYSNAME      ; Video out System Name
        lda #'#'|$80            ; Set Prompt '#'
        sta PROMPT
        lda #0
        sta $5

; ***************************************************
; Main monitor loop
; ***************************************************

L0      
        readline 23
        scroll
        clearline 22
        vout 22,0,BUFFER        ; Video out buffered line
        jsr GETTOK
        beq L0                  ; Not a token read next line
        clc
        asl a                   ; Multiply by 2
        tax
        lda JUMPTABLE,x
        sta JMPADR
        inx
        lda JUMPTABLE,x
        sta JMPADR+1       
        jsr SYSCALL
        jmp L0                  ; read next line



; ***************************************************
; RDLINE: Read line and echo to the screen  
;   INPUT   A screem row 0 .. 23
;   OUPUT   
;   AFFECTS A,X,Y,Z,N
; ***************************************************
;

RDLINE  
        jsr CLRLIN
        ldy #$00        ; Draw prompt and flashing blank
        lda PROMPT      
        echo
        iny
        input
        rts      

; ***************************************************
; INPUT: Input char and echo to the screen at 
;        (SCRADDR) Y pos 
;   INPUT   
;   OUPUT   
;   AFFECTS A,Y,Z,N
; ***************************************************
;

INPUT  
        ldx #0        
        lda #0          ; Clear the buffer
L11
        sta BUFFER,x
        inx
        cpx #41
        bne L11
        ldx #0
L12
        lda BUFFER,x    ; Load current buffered char
        bne L13
        lda #$60        ; At end position draw flashing blank 
L13
        echo
        jsr RDCHAR
        cmp #$8D        ; Return
        beq INPUT_
        cmp #$88
        beq LEFT
        cmp #$95
        beq RIGHT
        echo            ; echo current char
        and #$7F
        sta BUFFER,x    ; Store data to buffer
        iny
        inx
        jmp L12
INPUT_ 
        rts    
LEFT
        cpx #0
        bne L14         ; not a zero ok !
        jmp L12         ; already at zero left impossbile
L14        
        lda BUFFER,x    ; Load current buffered char
        bne L15
        lda #$A0        ; At end position print unflashing blank
        echo
        dey
        dex
        jmp L12
L15             
        ora #$80        ; Print unflashing char
        echo
        dey
        dex
        jmp L12
RIGHT   
        lda BUFFER,x    ; Load current buffered char
        beq L12
        ora #$80        ; Print unflashing char
        echo
        inx
        iny
        jmp L12


; ***************************************************
; CLRLIN: Clear screen line A 
;   INPUT   A screen row 0 .. 23
;   OUPUT   Video Text Page 1
;   AFFECTS Y
; ***************************************************
;
; Clear screen line #A
;

CLRLIN  
        pha
        jsr BASCALC
        ldy #00
        lda #$A0
L1      
        echo
        iny
        cpy #40
        bne L1
        pla
        rts

; ***************************************************
; SCROLL: Scroll screen 
;   INPUT   
;   OUPUT   
;   AFFECTS A,X
; ***************************************************
;
SCROLL  
        ldx #0
L7      
        lda $480,x
        sta $400,x
        lda $500,x
        sta $480,x
        lda $580,x
        sta $500,x
        lda $600,x
        sta $580,x        
        lda $680,x
        sta $600,x        
        lda $700,x
        sta $680,x        
        lda $780,x
        sta $700,x        
        lda $428,x
        sta $780,x        
        lda $4A8,x
        sta $428,x        
        lda $528,x
        sta $4A8,x        
        lda $5A8,x
        sta $528,x        
        lda $628,x
        sta $5A8,x        
        lda $6A8,x
        sta $628,x        
        lda $728,x
        sta $6A8,x        
        lda $7A8,x
        sta $728,x        
        lda $450,x
        sta $7A8,x        
        lda $4D0,x
        sta $450,x        
        lda $550,x
        sta $4D0,x        
        lda $5D0,x
        sta $550,x        
        lda $650,x
        sta $5D0,x        
        lda $6D0,x
        sta $650,x        
        lda $750,x
        sta $6D0,x        
        lda $7D0,x
        sta $750,x        
        inx
        cpx #40
        beq L9
        jmp L7
L9      rts
        

; ***************************************************
; CLRSCR: Clear screen 
;   INPUT   
;   OUPUT   Video Text Page 1
;   AFFECTS X
; ***************************************************
;

CLRSCR  
        pha  
        ldx #0
L2      
        txa
        jsr CLRLIN
        inx
        cmp #24
        bne L2
        pla
        rts

        
; ***************************************************
; RDCHAR: Read char from keyboard  
;   INPUT   
;   OUPUT   A
;   AFFECTS A,N,Z
; ***************************************************
;        
; Read a single char and returns its value in A
;

RDCHAR  lda KBRD
        bpl RDCHAR
        bit STROBE
        rts

; ***************************************************
; BASCALC: Compute the screen line base address
;   INPUT   A
;   OUPUT   SCRADDR,SCRADDR+1
;   AFFECTS A
; ***************************************************
;        
; Form Apple Monitor Peeled:  A is the screen line 0 .. 23
; A can be thought as  A = 000ABCDE
; The base address for text page 1 is: SCRADDR = 000001CD EABAB000
; The routine runs in 40 machine cycles                                 
;

BASCALC pha             ; A = 000ABCDE
        lsr a           ; A = 0000ABCD, C = E
        and #3          ; A = 000000CD
        ora #4          ; A = 000001CD
        sta SCRADDR+1   ; Store hi byte
        pla             ; A = 000ABCDE
        and #$18        ; A = 000AB000
        bcc L6          ; 
        adc #$7F        ; A = E00AB000
L6      sta SCRADDR     ; SCRADDR = E00AB000
        asl a           ; A = 00AB0000, C = E 
        asl a           ; A = 0AB00000, C = 0
        ora SCRADDR     ; A = EABAB000
        sta SCRADDR     ; SCRADDR = EABAB000
        rts

; ***************************************************
; GETTOK: String compare
;   INPUT   
;   OUPUT   A token number
;   AFFECTS A,X,Y,VAR1
; ***************************************************

GETTOK
        lda #0
        sta VAR1
        ldy #0
L20             
        ldx #0
L21        
        lda BUFFER,x
        bne L22         
        lda #$20        ; if end of input string substitute with space
L22
        cmp KEYWORD,y
        bne NOTTHIS
        cmp #$20        ; string are equal 
        beq TOKFOUND
        inx             ; compare next char
        iny
        jmp L21        
TOKFOUND        
        lda VAR1
        ora #$80
        rts
NOTTHIS
        inc VAR1
L23
        lda KEYWORD,y
        beq TOKNOTFOUND ; if keyword == 0 Token Not Found
        iny             ; else point to next keyword char
        cmp #$20         
        beq L20         ; If keyword == space Star check next keyword 
        jmp L23         ; Keep on search end of keyword
TOKNOTFOUND 
        lda #0          ; Token Not Found
        rts
         

; ***************************************************
; PRINTHEX: Print A ad hexadecimal at echo position
;   INPUT   
;   OUPUT   screen
;   AFFECTS A,N,Z,C
; ***************************************************
        
PRINTHEX
        iny             ; print low nibble first (increment y)
        pha             
        and #$0f        ; extract low nibble
        cmp #10         ; if nibble >= 10
        bcc L30        
        adc #6          ; add ascii 6
L30        
        adc #$B0        ; add ascii '0'
        echo
        dey             ; print high nibble first (decrement y)
        pla
        lsr a
        lsr a
        lsr a
        lsr a
        cmp #10
        bcc L31
        adc #6
L31        
        adc #$B0
        echo        
        rts
        
; ***************************************************
; HELLO: Print Hello message on row 22
;   INPUT   
;   OUPUT
;   AFFECTS 
; ***************************************************

HELLO
        scroll
        clearline 22
        vout 22,0,_S1   ; Video out hello message
        rts

; ***************************************************
; HELLO: Print Hello message on row 22
;   INPUT   
;   OUPUT
;   AFFECTS 
; ***************************************************

DUMP
        scroll
        clearline 22
        vout 22,0,_S2   ; Video out hello message
        rts
        
; ***************************************************
; HELLO: Print Hello message on row 22
;   INPUT   
;   OUPUT
;   AFFECTS 
; ***************************************************

GO
        scroll
        clearline 22
        vout 22,0,_S3   ; Video out hello message
        rts
        
; **************************************
; Jump table 
; **************************************
SYSCALL
        jmp (JMPADR)
JUMPTABLE 
        dw HELLO
        dw DUMP
        dw GO
        
; **************************************
; Strings 
; **************************************

_SYSNAME db "PPS-2",0
_S1      db "HELLO THIS IS PPS-2 SYSTEM MONITOR V1.0",0
_S2      db "THIS IS DUMP",0
_S3      db "THIS IS GO",0
KEYWORD  db "HELLO DUMP GO ",0 
 
; Interrupt vector
        org $FFFA

        dw $0000        ; NMI
        dw START        ; RESET
        dw $0000        ; IRQ
