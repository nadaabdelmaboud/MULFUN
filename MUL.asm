 .MODEL HUGE
 .STACK 64
 .DATA
 
startmsg db 'PRESS ENTER TO START','$'       
LIVES DB 3									 ;THE LIVES THE PLAYER HAS

;;;;;;;;;;;;;;;;;;;;;; WELLDONE IMAGE DATA
WELLHEIGHT EQU 100
WELLWIDTH EQU 200

XWELL DW 50
YWELL DW 50
WELLFilename DB 'WELL.bin', 0
WELLFilehandle DW ?
WELLData DB WELLWIDTH*WELLHEIGHT dup(0)
;;;;;;;;;;;;;;;;;;;;;;;

wi dw 0
he dw 0
BEGX DW ?
i db 1				; THE FIRST NUMBER
j db 1				; THE SECOND NUMBER
mes db 'ANSWER :','$'
ENTERmes DB 'ENTER','$'
SPACEmes DB '     ','$'
ANSWER DB 0,0,0
COUNTER DW 0
WIN DB 0
START db 0
XSQ DW ?
YSQ DW ?
NUMANSWER DB 0
WidthIMG equ 50
Height equ 50

exitgame db 0
;;;;;;;;;;;;;;;;;;;;;HEART IMAGE DATA
HEARTHEIGHT EQU 25
HEARTWIDTH EQU 25

XHEART DW 290
YHEART DW 175
HEARTFilename DB 'HEART.bin', 0
HEARTFilehandle DW ?
HEARTData DB HEARTWIDTH*HEARTHEIGHT dup(0)
;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;; MULTIPLICATION X IMAGE DATA
MULHEIGHT EQU 25
MULWIDTH EQU 25

XMUL DW 150
YMUL DW 30
MULFilename DB 'X.bin', 0
MULFilehandle DW ?
MULData DB MULWIDTH*MULHEIGHT dup(0)
;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;NUMBERS IMAGES DATA
XZERO DW 100
YZERO DW 30
ZEROFilename DB 'zero.bin', 0
ZEROFilehandle DW ?
ZEROData DB WidthIMG*Height dup(0)

XONE DW 100
YONE DW 30
OneFilename DB 'one.bin', 0
OneFilehandle DW ?
OneData DB WidthIMG*Height dup(0)

XTWO DW 100
YTWO DW 30
TWOFilename DB 'two.bin', 0
TWOFilehandle DW ?
TWOData DB WidthIMG*Height dup(0)

XTHREE DW 100
YTHREE DW 30
THREEFilename DB 'three.bin', 0
THREEFilehandle DW ?
THREEData DB WidthIMG*Height dup(0)

XFOUR DW 100
YFOUR DW 30
FOURFilename DB 'four.bin', 0
FOURFilehandle DW ?
FOURData DB WidthIMG*Height dup(0)

XFIVE DW 100
YFIVE DW 30
FIVEFilename DB 'five.bin', 0
FIVEFilehandle DW ?
FIVEData DB WidthIMG*Height dup(0)

XSIX DW 100
YSIX DW 30
SIXFilename DB 'six.bin', 0
SIXFilehandle DW ?
SIXData DB WidthIMG*Height dup(0)

XSEVEN DW 100
YSEVEN DW 30
SEVENFilename DB 'seven.bin', 0
SEVENFilehandle DW ?
SEVENData DB WidthIMG*Height dup(0)

XEIGHT DW 100
YEIGHT DW 30
EIGHTFilename DB 'eight.bin', 0
EIGHTFilehandle DW ?
EIGHTData DB WidthIMG*Height dup(0)

XNINE DW 100
YNINE DW 30
NINEFilename DB 'nine.bin', 0
NINEFilehandle DW ?
NINEData DB WidthIMG*Height dup(0)

XTEN DW 100
YTEN DW 30
TENFilename DB 'ten.bin', 0
TENFilehandle DW ?
TENData DB WidthIMG*Height dup(0)
;;;;;;;;;;;;;;;;;;

.CODE

MAIN PROC FAR
        Mov Ax,@DATA
        MOV DS,Ax
;;;;;;;;;;;;;;;;;;;;; CHANGE VIDEO MODE        
mov ah,0
mov al,13h
int 10h
;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;; FLAG TO CHECK END OF GAME
EXITGAMEFLAG4: 
	CMP LIVES,0
	JNZ CONTPLAY
	MOV CX,50
	MOV XWELL,CX
	MOV YWELL,CX
	CALL DRAWWELL
;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;; START GAME
CONTPLAY:

	CALL CLEARSCREEN
	CALL RESET      ;RESET VARIABLES USED IN THE GAME
	CALL MENU		;MAIN MENU
	CALL CLEARSCREEN
	CALL DRAWSTATUSBAR
;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;; LOOPS OF THE MULTIPLE NUMBERS
OuterLoop:				;THE START OF EACH MULTIPLICATION TABLE (J=1)
	CALL ClearScreenGAME
	mov al,1
	mov j,al
	mov al,0
	mov start,al
	MOV AX,150
	MOV XMUL,AX
	mov ax,80           ;AX HOLDS THE X AXIS
	mov dx,30			;DX HOLDS THE Y AXIS
	MOV YMUL,DX
	jmp checkone
CONTOUTJ:				;THE START WITHIN EACH TABLE (1<J<12)
	CALL ClearScreenGAME
	mov dx,30
	mov cl,0
	mov start,cl
	MOV YMUL,DX
	jmp checkone
CONTOUTER:				;THE MOVEMENT THROUGH ONE ITERATION
	CALL ClearScreenGAME
	ADD dx,20
	mov cl,1
	mov start,CL
	MOV YMUL,DX
checkone:				;CHECK IF THE FIRST NUMBER (i)=1
	CALL DRAWMUL		;DRAW THE X SYMBOL
	mov ax,80			;MOV 80 TO X AXIS
	cmp i,1				;CHECK IF 1
	jnz checktwo		; MOVE TO NEXT NUMBER IF NO
	mov xone,ax			;MOVE AX TO X OF THE ONE IMAGE
	mov yone,dx			;MOVE DX TO Y OF THE ONE IMAGE
	call drawone		;CALL THE DRAW ONE PROC
	jmp continner		;JUMB TO THE INNER LOOP FOR THE SECOND NUMBER
checktwo:				;CHECK IF THE FIRST NUMBER (i)=2
	cmp i,2
	jnz checkthree
	mov xtwo,ax
	mov ytwo,dx
	call drawtwo
	jmp continner
checkthree:				;CHECK IF THE FIRST NUMBER (i)=3
	cmp i,3
	jnz checkfour
	mov xthree,ax
	mov ythree,dx
	call drawthree
	jmp continner
checkfour:				;CHECK IF THE FIRST NUMBER (i)=4
	cmp i,4
	jnz checkfive
	mov xfour,ax
	mov yfour,dx
	call drawfour
	jmp continner
checkfive:				;CHECK IF THE FIRST NUMBER (i)=5
	cmp i,5
	jnz checksix
	mov xfive,ax
	mov yfive,dx
	call drawfive
	jmp continner
EXITGAMEFLAG3:
JMP EXITGAMEFLAG4
checksix:				;CHECK IF THE FIRST NUMBER (i)=6
	cmp i,6
	jnz checkseven
	mov xsix,ax
	mov ysix,dx
	call drawsix
	jmp continner
checkseven:				;CHECK IF THE FIRST NUMBER (i)=7
	cmp i,7
	jnz checkeight
	mov xseven,ax
	mov yseven,dx
	call drawseven
	jmp continner	
checkeight:				;CHECK IF THE FIRST NUMBER (i)=8
	cmp i,8
	jnz checknine
	mov xeight,ax
	mov yeight,dx
	call draweight
	jmp continner
checknine:				;CHECK IF THE FIRST NUMBER (i)=9
	cmp i,9
	jnz checkten
	mov xnine,ax
	mov ynine,dx
	call drawnine
	jmp continner
checkten:				;CHECK IF THE FIRST NUMBER (i)=10
	cmp i,10
	jnz checkeleven
	mov XTEN,ax
	mov YTEN,dx
	call drawten
	jmp continner
checkeleven:			;CHECK IF THE FIRST NUMBER (i)=11
	cmp i,11
	jnz checktwelve
	mov xone,ax
	mov yone,dx
	call drawone
	add XONE,40
	call drawone
	jmp continner
checktwelve:			;CHECK IF THE FIRST NUMBER (i)=12
	cmp i,12
	jnz checkeleven
	mov xone,ax
	mov yone,dx
	mov xtwo,ax
	mov ytwo,dx
	call drawone
	add XTWO,40
	call drawtwo
	jmp continner
;;;;;;;;;;;;;;;;;;;;LOOP OF THE SECOND NUMBER
continner:
	cmp start,0		;CHECK IF START =0 TO GET THE Y AXIS (DX) TO THE BEGINNING OR NOT
	je InnerLoop
	jmp INcheckone
InnerLoop:
	mov ax,240
	mov dx,30
	jmp INcheckone
	EXITGAMEFLAG2:
	JMP EXITGAMEFLAG3
INcheckone:			;CHECK IF THE SECOND NUMBER (J)=1
	mov ax,240
	cmp j,1
	jnz INchecktwo
	mov xone,ax
	mov yone,dx
	call drawone
	jmp INcontinner
INchecktwo:			;CHECK IF THE SECOND NUMBER (J)=2
	cmp j,2
	jnz INcheckthree
	mov xtwo,ax
	mov ytwo,dx
	call drawtwo
	jmp INcontinner
INcheckthree:		;CHECK IF THE SECOND NUMBER (J)=3
	cmp j,3
	jnz INcheckfour
	mov xthree,ax
	mov ythree,dx
	call drawthree
	jmp INcontinner
INcheckfour:		;CHECK IF THE SECOND NUMBER (J)=4
	cmp j,4
	jnz INcheckfive
	mov xfour,ax
	mov yfour,dx
	call drawfour
	jmp INcontinner
INcheckfive:		;CHECK IF THE SECOND NUMBER (J)=5
	cmp j,5
	jnz INchecksix
	mov xfive,ax
	mov yfive,dx
	call drawfive
	jmp INcontinner
INchecksix:			;CHECK IF THE SECOND NUMBER (J)=6
	cmp j,6
	jnz INcheckseven
	mov xsix,ax
	mov ysix,dx
	call drawsix
	jmp INcontinner
INcheckseven:		;CHECK IF THE SECOND NUMBER (J)=7
	cmp j,7
	jnz INcheckeight
	mov xseven,ax
	mov yseven,dx
	call drawseven
	jmp INcontinner	
INcheckeight:		;CHECK IF THE SECOND NUMBER (J)=8
	cmp j,8
	jnz INchecknine
	mov xeight,ax
	mov yeight,dx
	call draweight
	jmp INcontinner
INchecknine:		;CHECK IF THE SECOND NUMBER (J)=9
	cmp j,9
	jnz INcheckten
	mov xnine,ax
	mov ynine,dx
	call drawnine
	jmp INcontinner
	CONTOUTERFlag1:
    jmp CONTOUTER
	EXITGAMEFLAG:
	JMP EXITGAMEFLAG2
INcheckten:			;CHECK IF THE SECOND NUMBER (J)=10
	cmp j,10
	jnz INcheckeleven
	mov xten,ax
	mov yten,dx
	call drawten
	jmp INcontinner
INcheckeleven:		;CHECK IF THE SECOND NUMBER (J)=11
	cmp j,11
	jnz INchecktwelve
	mov xone,ax
	mov yone,dx
	call drawone
	add XONE,40
	call drawone
	jmp INcontinner
INchecktwelve:		;CHECK IF THE SECOND NUMBER (J)=12
	cmp j,12
	jnz EXITPROG
	mov xone,ax
	mov yone,dx
	mov xtwo,ax
	mov ytwo,dx
	call drawone
	add XTWO,40
	call drawtwo
	jmp INcontinner

INcontinner:		;CONTINUE THE INNER LOOP
 call delay			; DELAY 1 SECOND BETWEEN EACH MOVEMENT (CHECK FOR ANY KEY PRESSED TOO IN THE DELAY PROC)
 CMP EXITGAME,1		; IF THE ESC KEY IS PRESSED GO BACK TO THE MAIN MENU
 jz EXITGAMEFLAG	
 CMP WIN,1			;IF THE CORRECT ANSWER IS ENTERED THEN MOVE TO THE NEXT PROBLEM (INC J OR MOVE TO THE NEXT I IF J=12)
 JZ CHECKWIN
 CMP DX,150			;IF THE NUMBERS GET TO THE END OF THE SCREEN THEN DECREMENT THE LIVES AND START THE CURRENT MUL. TABLE FROM THE BEGINNING
 JNZ CONTOUTERFlag1
 DEC LIVES
 CMP LIVES,0
 JZ EXITGAMEFLAG
 JMP CONTG
EXITWIN:
	CALL DRAWWELL
	JMP EXITGAMEFLAG
CONTG:
	CALL DRAWSTATUSBAR
	dec i
	jmp loopilose
CHECKWIN:
	DEC WIN
	cmp j,12
	jz loopi
	inc j
	jmp CONTOUTJ 
loopi:
	CALL DRAWWELL
	CALL DRAWSTATUSBAR
	cmp i,12
	jz EXITWIN
loopilose:
	inc i
	jmp OUTERLOOP


Exitprog:	
		 MOV AH,4CH
		 INT 21H

MAIN ENDP

DRAWMUL PROC NEAR		;DRAW X SYMBOL PROCEDURE
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    ; Open file

    MOV AH, 3Dh
    MOV AL, 0 ; read only
    LEA DX, MULFilename
    INT 21h
    MOV [MULFilehandle], AX
    ;;;;;;;;;;;read data
    MOV AH,3Fh
    MOV BX, [MULFilehandle]
    MOV CX,MULWIDTH*MULHEIGHT ; number of bytes to read
    LEA DX, MULData
    INT 21h
    ;;;;;;;;;;;;;;;;;;
	
    LEA BX , MULData ; BL contains index at the current drawn pixel
    MOV DX,YMUL
    MOV  CX,XMUL
    MOV he,dx
    MOV wi,cx
    add he,25
    add wi,25
   
    MOV BEGX,CX
    MOV AH,0ch
  
; Drawing loop
drawLoopMUL:
    MOV AL,[BX]
    INT 10h 
    INC CX
    INC BX
    CMP CX,wi
JNE drawLoopMUL
    MOV CX ,BEGX	
    INC DX
    CMP DX , he
JNE drawLoopMUL

    ;;;;;;;;;;;;;;;;close file
    MOV AH, 3Eh
    MOV BX, [MULFilehandle]
    INT 21h

    POP DX
    POP CX
    POP BX
    POP AX
RET
DRAWMUL ENDP

MENU PROC NEAR		;DRAW MAIN MENU PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX

		
		MOV AX,290
		MOV DX,50
		MOV XONE,AX
		MOV YONE,DX
		CALL DRAWONE
		LOOPON:
		MOV BX,XONE
		MOV CX,YONE
		MOV XSQ,BX
		MOV YSQ,CX
		CALL DRAWSQUARE
		SUB XONE,50
		CALL DRAWONE
		CALL DELAYNUM
		CMP XONE,40
		JNZ LOOPON
		
		MOV XTWO,AX
		MOV YTWO,DX
		CALL DRAWTWO
		LOOPTW:
		MOV BX,XTWO
		MOV CX,YTWO
		MOV XSQ,BX
		MOV YSQ,CX
		CALL DRAWSQUARE
		SUB XTWO,50
		CALL DRAWTWO
		CALL DELAYNUM
		CMP XTWO,90
		JNZ LOOPTW
		
		OUTTWO:
		MOV XTHREE,AX
		MOV YTHREE,DX
		CALL DRAWTHREE
		LOOPTH:
		MOV BX,XTHREE
		MOV CX,YTHREE
		MOV XSQ,BX
		MOV YSQ,CX
		CALL DRAWSQUARE
		SUB XTHREE,50
		CALL DRAWTHREE
		CALL DELAYNUM
		CMP XTHREE,140
		JNZ LOOPTH
		
		OUTTH:
	    MOV XFOUR,AX
		MOV YFOUR,DX
		CALL DRAWFOUR
		LOOPF:
		MOV BX,XFOUR
		MOV CX,YFOUR
		MOV XSQ,BX
		MOV YSQ,CX
		CALL DRAWSQUARE
		SUB XFOUR,50
		CALL DRAWFOUR
		CALL DELAYNUM
		CMP XFOUR,190
		JNZ LOOPF

	OUTFO:
	    MOV XFIVE,AX
		MOV YFIVE,DX
		CALL DRAWFIVE
		LOOPFI:
		MOV BX,XFIVE
		MOV CX,YFIVE
		MOV XSQ,BX
		MOV YSQ,CX
		CALL DRAWSQUARE
		SUB XFIVE,50
		CALL DRAWFIVE
		CALL DELAYNUM
		CMP XFIVE,240
		JNZ LOOPFI
		
		
		MOV XSIX,AX
		MOV YSIX,DX
		CALL DRAWSIX
		
		MOV CX,102
		MOV BX,130
		MOV XSQ,CX
		MOV YSQ,BX
		CALL DRAWBLACKSQUARE
		
		MOV BH,0H
	    mov ah,2
	    mov dH,14
		MOV DL,10
		int 10h
		
		mov ah, 09H
		mov dx, offset startmsg
		int 21h
press:
	MOV AH,0
	INT 16H
	cmp ah,01h
	jnz exitgam
	MOV AH,4CH
	INT 21H	
exitgam:
	cmp ah,01ch
	jnz press
	
    POP DX
    POP CX
    POP BX
    POP AX
		ret
MENU ENDP
DelayNUM PROC NEAR   ;DELAY BETWEEN THE MOVEMENT OF THE NUMBER IN THE MAIN MENU RUN
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
	MOV BX,600
	LB:
	MOV CX,600
	LC:
	LOOP LC
	DEC BX
	JNZ LB
    POP DX
    POP CX
    POP BX
    POP AX
		ret
DelayNUM ENDP

Delay PROC NEAR		;DELAY BETWEEN EACH MOVEMENT OF NUMBERS THROUGH THE GAME
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
		mov bh,01h                 ;number of seconds for delay
		mov ah,2ch
		int 21h
		mov [Di],dh                ;first time
		
		
		
	compare:
		mov ah,1		;CHECK IF ANY KEY IS PRESSED
		int 16h
		JZ CONTT
		CMP AH,01H		;IF THE ESC THEN SET THE EXITGAME FLAG AND RETURN
		JZ EXITALL
		;;;;;;;;;;;CHECK IF THE KEY IS A NUMBER OR THE ENTER KEY
		CMP AH,02H		
		JL CONTT
		CMP AH,0BH
		JG CHECKENTER
		CALL READ	;READ PROC TO READ THE PRESSED KEY AND STORE IT IN THE ANSWER VAR AND IF ENTER IS THE PRESSED KEY IT CALLS THE CHECK ANSWER PROC
		MOV AX,0	;FREE THE KEYBOARD BUFFER
		INT 16H
		JMP CONTT
		CHECKENTER:
		CMP AH,01CH
		JNZ CONTTENT
		CALL READ
	CONTTENT:
    	MOV AX,0
		INT 16H
	CONTT:
		mov ah,2ch
    	int 21h                   ;call int 21h to get the time again   
		sub dh,[DI]               ;subtract the current time with 
		Cmp dh,bh                 ;compare the time passed with the delay time
		jb compare
		JMP DEL
	EXITALL:
		 MOV AH,1
		 mov exitgame ,ah
		 MOV AX,0
		 INT 16H
	DEL:
    POP DX
    POP CX
    POP BX
    POP AX
		ret
Delay ENDP

DelayWELL PROC NEAR   ;DELAY FOR SHOWING THE WELL DONE IMAGE 
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
		mov bh,01h                 ;number of seconds for delay
		mov ah,2ch
		int 21h
		mov [Di],dh                ;first time
		
		compareWELL:
    	int 21h                   ;call int 21h to get the time again   
		sub dh,[DI]               ;subtract the current time with 
		Cmp dh,bh                 ;compare the time passed with the delay time
		jb compareWELL

    POP DX
    POP CX
    POP BX
    POP AX
		ret
DelayWELL ENDP

DRAWHEART PROC NEAR
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    ; Open file

    MOV AH, 3Dh
    MOV AL, 0 ; read only
    LEA DX, HEARTFilename
    INT 21h
    MOV [HEARTFilehandle], AX
    ;;;;;;;;;;;read data
    MOV AH,3Fh
    MOV BX, [HEARTFilehandle]
    MOV CX,HEARTWIDTH*HEARTHEIGHT ; number of bytes to read
    LEA DX, HEARTData
    INT 21h
    ;;;;;;;;;;;;;;;;;;
	
    LEA BX , HEARTData ; BL contains index at the current drawn pixel
    MOV DX,YHEART
    MOV  CX,XHEART
    MOV he,dx
    MOV wi,cx
    add he,25
    add wi,25

    MOV BEGX,CX
    MOV AH,0ch
  
; Drawing loop
drawLoopHEART:
    MOV AL,[BX]
    INT 10h 
    INC CX
    INC BX
    CMP CX,wi
JNE drawLoopHEART
    MOV CX ,BEGX	
    INC DX
    CMP DX , he
JNE drawLoopHEART

    ;;;;;;;;;;;;;;;;close file
    MOV AH, 3Eh
    MOV BX, [HEARTFilehandle]
    INT 21h

    POP DX
    POP CX
    POP BX
    POP AX
RET
DRAWHEART ENDP

LOOPHEARTS PROC NEAR  ;LOOP TO DRAW HEARTS FOR NUMBER OF LIVES 
PUSH AX
PUSH BX
PUSH CX
PUSH DX
MOV AX,290
MOV XHEART,AX
MOV CH,0
MOV CL,LIVES

HTL:
CALL DRAWHEART
SUB XHEART,25
LOOP HTL


POP DX
POP CX
POP BX
POP AX
RET
LOOPHEARTS ENDP

DRAWSTATUSBAR proc NEAR
PUSH AX
PUSH BX
PUSH CX
PUSH DX
    mov ah,0ch
    MOV DX,175
	MOV AL,00H
	YBAR:
            MOV CX,0    
            XBAR:
            INT 10H
            INC CX
            CMP CX,320       	
            JNZ XBAR      
            INC DX
            CMP DX,200
        JNZ YBAR 
	MOV BH,0H
	mov ah,2
	mov dH,23
	MOV DL,1
	int 10h	
	
mov ah, 09H
mov dx, offset mes
int 21h

CALL LOOPHEARTS

    POP DX
    POP CX
    POP BX
    POP AX
	RET
DRAWSTATUSBAR ENDP
RESET PROC NEAR ; RESET THE GAME VARIABLES
PUSH AX
PUSH BX
PUSH CX

MOV AL,1
MOV I,AL
MOV J,AL
MOV BL,0
MOV CX,0
mov EXITGAME,bl
LEA SI,ANSWER
MOV [SI],BL
INC SI
MOV [SI],BL
INC SI
MOV [SI],BL
MOV COUNTER,CX
MOV WIN,CL
MOV START,CL
MOV NUMANSWER,CL
MOV CL,3
MOV LIVES,CL
MOV CX,50
MOV XWELL,CX
MOV YWELL,CX

POP CX
POP BX
POP AX
RET
RESET ENDP

DRAWZERO PROC NEAR
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    ; Open file

    MOV AH, 3Dh
    MOV AL, 0 ; read only
    LEA DX, ZEROFilename
    INT 21h
    MOV [ZEROFilehandle], AX
    ;;;;;;;;;;;read data
    MOV AH,3Fh
    MOV BX, [ZEROFilehandle]
    MOV CX,WidthIMG*Height ; number of bytes to read
    LEA DX, ZEROData
    INT 21h
    ;;;;;;;;;;;;;;;;;;
	
    LEA BX , ZEROData ; BL contains index at the current drawn pixel
    MOV DX,YZERO
    MOV  CX,XZERO
    MOV he,dx
    MOV wi,cx
    add he,25
    add wi,25
    SUB CX,25       ;;;;;;ADJUST TO TOP LEFT
    SUB DX,25
    MOV BEGX,CX
    MOV AH,0ch
  
; Drawing loop
drawLoopZ:
    MOV AL,[BX]
    INT 10h 
    INC CX
    INC BX
    CMP CX,wi
JNE drawLoopZ	
    MOV CX ,BEGX	
    INC DX
    CMP DX , he
JNE drawLoopZ

    ;;;;;;;;;;;;;;;;close file
    MOV AH, 3Eh
    MOV BX, [ZEROFilehandle]
    INT 21h

    POP DX
    POP CX
    POP BX
    POP AX
RET
DRAWZERO ENDP

DRAWONE PROC NEAR
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    ; Open file

    MOV AH, 3Dh
    MOV AL, 0 ; read only
    LEA DX, ONEFilename
    INT 21h
    MOV [ONEFilehandle], AX
    ;;;;;;;;;;;read data
    MOV AH,3Fh
    MOV BX, [ONEFilehandle]
    MOV CX,WidthIMG*Height ; number of bytes to read
    LEA DX, ONEData
    INT 21h
    ;;;;;;;;;;;;;;;;;;
	
    LEA BX , ONEData ; BL contains index at the current drawn pixel
    MOV DX,YONE
    MOV  CX,XONE
    MOV he,dx
    MOV wi,cx
    add he,25
    add wi,25
    SUB CX,25       ;;;;;;ADJUST TO TOP LEFT
    SUB DX,25
    MOV BEGX,CX
    MOV AH,0ch
  
; Drawing loop
drawLoopT:
    MOV AL,[BX]
    INT 10h 
    INC CX
    INC BX
    CMP CX,wi
JNE drawLoopT	
    MOV CX ,BEGX	
    INC DX
    CMP DX , he
JNE drawLoopT

    ;;;;;;;;;;;;;;;;close file
    MOV AH, 3Eh
    MOV BX, [ONEFilehandle]
    INT 21h

    POP DX
    POP CX
    POP BX
    POP AX
RET
DRAWONE ENDP

DRAWTWO PROC NEAR
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    ; Open file

    MOV AH, 3Dh
    MOV AL, 0 ; read only
    LEA DX, TWOFilename
    INT 21h
    MOV [TWOFilehandle], AX
    ;;;;;;;;;;;read data
    MOV AH,3Fh
    MOV BX, [TWOFilehandle]
    MOV CX,WidthIMG*Height ; number of bytes to read
    LEA DX, TWOData
    INT 21h
    ;;;;;;;;;;;;;;;;;;
	
    LEA BX , TWOData ; BL contains index at the current drawn pixel
    MOV DX,YTWO
    MOV  CX,XTWO
    MOV he,dx
    MOV wi,cx
    add he,25
    add wi,25
    SUB CX,25       ;;;;;;ADJUST TO TOP LEFT
    SUB DX,25
    MOV BEGX,CX
    MOV AH,0ch
  
; Drawing loop
drawLoopTWO:
    MOV AL,[BX]
    INT 10h 
    INC CX
    INC BX
    CMP CX,wi
JNE drawLoopTWO	
    MOV CX ,BEGX	
    INC DX
    CMP DX , he
JNE drawLoopTWO

    ;;;;;;;;;;;;;;;;close file
    MOV AH, 3Eh
    MOV BX, [TWOFilehandle]
    INT 21h

    POP DX
    POP CX
    POP BX
    POP AX
RET
DRAWTWO ENDP

DRAWTHREE PROC NEAR
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    ; Open file

    MOV AH, 3Dh
    MOV AL, 0 ; read only
    LEA DX, THREEFilename
    INT 21h
    MOV [THREEFilehandle], AX
    ;;;;;;;;;;;read data
    MOV AH,3Fh
    MOV BX, [THREEFilehandle]
    MOV CX,WidthIMG*Height ; number of bytes to read
    LEA DX, THREEData
    INT 21h
    ;;;;;;;;;;;;;;;;;;
	
    LEA BX , THREEData ; BL contains index at the current drawn pixel
    MOV DX,YTHREE
    MOV  CX,XTHREE
    MOV he,dx
    MOV wi,cx
    add he,25
    add wi,25
    SUB CX,25       ;;;;;;ADJUST TO TOP LEFT
    SUB DX,25
    MOV BEGX,CX
    MOV AH,0ch
  
; Drawing loop
drawLoopTHREE:
    MOV AL,[BX]
    INT 10h 
    INC CX
    INC BX
    CMP CX,wi
JNE drawLoopTHREE	
    MOV CX ,BEGX	
    INC DX
    CMP DX , he
JNE drawLoopTHREE

    ;;;;;;;;;;;;;;;;close file
    MOV AH, 3Eh
    MOV BX, [THREEFilehandle]
    INT 21h

    POP DX
    POP CX
    POP BX
    POP AX
RET
DRAWTHREE ENDP

DRAWFOUR PROC NEAR
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    ; Open file

    MOV AH, 3Dh
    MOV AL, 0 ; read only
    LEA DX, FOURFilename
    INT 21h
    MOV [FOURFilehandle], AX
    ;;;;;;;;;;;read data
    MOV AH,3Fh
    MOV BX, [FOURFilehandle]
    MOV CX,WidthIMG*Height ; number of bytes to read
    LEA DX, FOURData
    INT 21h
    ;;;;;;;;;;;;;;;;;;
	
    LEA BX , FOURData ; BL contains index at the current drawn pixel
    MOV DX,YFOUR
    MOV  CX,XFOUR
    MOV he,dx
    MOV wi,cx
    add he,25
    add wi,25
    SUB CX,25       ;;;;;;ADJUST TO TOP LEFT
    SUB DX,25
    MOV BEGX,CX
    MOV AH,0ch
  
; Drawing loop
drawLoopFOUR:
    MOV AL,[BX]
    INT 10h 
    INC CX
    INC BX
    CMP CX,wi
JNE drawLoopFOUR	
    MOV CX ,BEGX	
    INC DX
    CMP DX , he
JNE drawLoopFOUR

    ;;;;;;;;;;;;;;;;close file
    MOV AH, 3Eh
    MOV BX, [FOURFilehandle]
    INT 21h

    POP DX
    POP CX
    POP BX
    POP AX
RET
DRAWFOUR ENDP

DRAWFIVE PROC NEAR
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    ; Open file

    MOV AH, 3Dh
    MOV AL, 0 ; read only
    LEA DX, FIVEFilename
    INT 21h
    MOV [FIVEFilehandle], AX
    ;;;;;;;;;;;read data
    MOV AH,3Fh
    MOV BX, [FIVEFilehandle]
    MOV CX,WidthIMG*Height ; number of bytes to read
    LEA DX, FIVEData
    INT 21h
    ;;;;;;;;;;;;;;;;;;
	
    LEA BX , FIVEData ; BL contains index at the current drawn pixel
    MOV DX,YFIVE
    MOV  CX,XFIVE
    MOV he,dx
    MOV wi,cx
    add he,25
    add wi,25
    SUB CX,25       ;;;;;;ADJUST TO TOP LEFT
    SUB DX,25
    MOV BEGX,CX
    MOV AH,0ch
  
; Drawing loop
drawLoopFIVE:
    MOV AL,[BX]
    INT 10h 
    INC CX
    INC BX
    CMP CX,wi
JNE drawLoopFIVE	
    MOV CX ,BEGX	
    INC DX
    CMP DX , he
JNE drawLoopFIVE

    ;;;;;;;;;;;;;;;;close file
    MOV AH, 3Eh
    MOV BX, [FIVEFilehandle]
    INT 21h

    POP DX
    POP CX
    POP BX
    POP AX
RET
DRAWFIVE ENDP

DRAWSIX PROC NEAR
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    ; Open file

    MOV AH, 3Dh
    MOV AL, 0 ; read only
    LEA DX, SIXFilename
    INT 21h
    MOV [SIXFilehandle], AX
    ;;;;;;;;;;;read data
    MOV AH,3Fh
    MOV BX, [SIXFilehandle]
    MOV CX,WidthIMG*Height ; number of bytes to read
    LEA DX, SIXData
    INT 21h
    ;;;;;;;;;;;;;;;;;;
	
    LEA BX , SIXData ; BL contains index at the current drawn pixel
    MOV DX,YSIX
    MOV  CX,XSIX
    MOV he,dx
    MOV wi,cx
    add he,25
    add wi,25
    SUB CX,25       ;;;;;;ADJUST TO TOP LEFT
    SUB DX,25
    MOV BEGX,CX
    MOV AH,0ch
  
; Drawing loop
drawLoopSIX:
    MOV AL,[BX]
    INT 10h 
    INC CX
    INC BX
    CMP CX,wi
JNE drawLoopSIX	
    MOV CX ,BEGX	
    INC DX
    CMP DX , he
JNE drawLoopSIX

    ;;;;;;;;;;;;;;;;close file
    MOV AH, 3Eh
    MOV BX, [SIXFilehandle]
    INT 21h

    POP DX
    POP CX
    POP BX
    POP AX
RET
DRAWSIX ENDP

DRAWSEVEN PROC NEAR
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    ; Open file

    MOV AH, 3Dh
    MOV AL, 0 ; read only
    LEA DX, SEVENFilename
    INT 21h
    MOV [SEVENFilehandle], AX
    ;;;;;;;;;;;read data
    MOV AH,3Fh
    MOV BX, [SEVENFilehandle]
    MOV CX,WidthIMG*Height ; number of bytes to read
    LEA DX, SEVENData
    INT 21h
    ;;;;;;;;;;;;;;;;;;
	
    LEA BX , SEVENData ; BL contains index at the current drawn pixel
    MOV DX,YSEVEN
    MOV  CX,XSEVEN
    MOV he,dx
    MOV wi,cx
    add he,25
    add wi,25
    SUB CX,25       ;;;;;;ADJUST TO TOP LEFT
    SUB DX,25
    MOV BEGX,CX
    MOV AH,0ch
  
; Drawing loop
drawLoopSEVEN:
    MOV AL,[BX]
    INT 10h 
    INC CX
    INC BX
    CMP CX,wi
JNE drawLoopSEVEN
    MOV CX ,BEGX	
    INC DX
    CMP DX , he
JNE drawLoopSEVEN

    ;;;;;;;;;;;;;;;;close file
    MOV AH, 3Eh
    MOV BX, [SEVENFilehandle]
    INT 21h

    POP DX
    POP CX
    POP BX
    POP AX
RET
DRAWSEVEN ENDP

DRAWEIGHT PROC NEAR
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    ; Open file

    MOV AH, 3Dh
    MOV AL, 0 ; read only
    LEA DX, EIGHTFilename
    INT 21h
    MOV [EIGHTFilehandle], AX
    ;;;;;;;;;;;read data
    MOV AH,3Fh
    MOV BX, [EIGHTFilehandle]
    MOV CX,WidthIMG*Height ; number of bytes to read
    LEA DX, EIGHTData
    INT 21h
    ;;;;;;;;;;;;;;;;;;
	
    LEA BX , EIGHTData ; BL contains index at the current drawn pixel
    MOV DX,YEIGHT
    MOV  CX,XEIGHT
    MOV he,dx
    MOV wi,cx
    add he,25
    add wi,25
    SUB CX,25       ;;;;;;ADJUST TO TOP LEFT
    SUB DX,25
    MOV BEGX,CX
    MOV AH,0ch
  
; Drawing loop
drawLoopEIGHT:
    MOV AL,[BX]
    INT 10h 
    INC CX
    INC BX
    CMP CX,wi
JNE drawLoopEIGHT
    MOV CX ,BEGX	
    INC DX
    CMP DX , he
JNE drawLoopEIGHT

    ;;;;;;;;;;;;;;;;close file
    MOV AH, 3Eh
    MOV BX, [EIGHTFilehandle]
    INT 21h

    POP DX
    POP CX
    POP BX
    POP AX
RET
DRAWEIGHT ENDP

DRAWNINE PROC NEAR
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    ; Open file

    MOV AH, 3Dh
    MOV AL, 0 ; read only
    LEA DX, NINEFilename
    INT 21h
    MOV [NINEFilehandle], AX
    ;;;;;;;;;;;read data
    MOV AH,3Fh
    MOV BX, [NINEFilehandle]
    MOV CX,WidthIMG*Height ; number of bytes to read
    LEA DX, NINEData
    INT 21h
    ;;;;;;;;;;;;;;;;;;
	
    LEA BX , NINEData ; BL contains index at the current drawn pixel
    MOV DX,YNINE
    MOV  CX,XNINE
    MOV he,dx
    MOV wi,cx
    add he,25
    add wi,25
    SUB CX,25       ;;;;;;ADJUST TO TOP LEFT
    SUB DX,25
    MOV BEGX,CX
    MOV AH,0ch
  
; Drawing loop
drawLoopNINE:
    MOV AL,[BX]
    INT 10h 
    INC CX
    INC BX
    CMP CX,wi
JNE drawLoopNINE
    MOV CX ,BEGX	
    INC DX
    CMP DX , he
JNE drawLoopNINE

    ;;;;;;;;;;;;;;;;close file
    MOV AH, 3Eh
    MOV BX, [NINEFilehandle]
    INT 21h

    POP DX
    POP CX
    POP BX
    POP AX
RET
DRAWNINE ENDP

DRAWTEN PROC NEAR
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    ; Open file

    MOV AH, 3Dh
    MOV AL, 0 ; read only
    LEA DX, TENFilename
    INT 21h
    MOV [TENFilehandle], AX
    ;;;;;;;;;;;read data
    MOV AH,3Fh
    MOV BX, [TENFilehandle]
    MOV CX,WidthIMG*Height ; number of bytes to read
    LEA DX, TENData
    INT 21h
    ;;;;;;;;;;;;;;;;;;
	
    LEA BX , TENData ; BL contains index at the current drawn pixel
    MOV DX,YTEN
    MOV  CX,XTEN
    MOV he,dx
    MOV wi,cx
    add he,25
    add wi,25
    SUB CX,25       ;;;;;;ADJUST TO TOP LEFT
    SUB DX,25
    MOV BEGX,CX
    MOV AH,0ch
  
; Drawing loop
drawLoopTEN:
    MOV AL,[BX]
    INT 10h 
    INC CX
    INC BX
    CMP CX,wi
JNE drawLoopTEN
    MOV CX ,BEGX	
    INC DX
    CMP DX , he
JNE drawLoopTEN

    ;;;;;;;;;;;;;;;;close file
    MOV AH, 3Eh
    MOV BX, [TENFilehandle]
    INT 21h

    POP DX
    POP CX
    POP BX
    POP AX
RET
DRAWTEN ENDP

DRAWWELL PROC NEAR
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    ; Open file
	CALL CLEARSCREEN
    MOV AH, 3Dh
    MOV AL, 0 ; read only
    LEA DX, WELLFilename
    INT 21h
    MOV [WELLFilehandle], AX
    ;;;;;;;;;;;read data
    MOV AH,3Fh
    MOV BX, [WELLFilehandle]
    MOV CX,WELLWIDTH*WELLHEIGHT ; number of bytes to read
    LEA DX, WELLData
    INT 21h
    ;;;;;;;;;;;;;;;;;;
	
    LEA BX , WELLData ; BL contains index at the current drawn pixel
    MOV DX,YWELL
    MOV CX,XWELL
    MOV he,dx
    MOV wi,cx
    add he,100
    add wi,200
    MOV BEGX,CX
    MOV AH,0ch
  
; Drawing loop
drawLoopWELL:
    MOV AL,[BX]
    INT 10h 
    INC CX
    INC BX
    CMP CX,wi
JNE drawLoopWELL
    MOV CX ,BEGX	
    INC DX
    CMP DX,he
JNE drawLoopWELL

    ;;;;;;;;;;;;;;;;close file
    MOV AH, 3Eh
    MOV BX, [WELLFilehandle]
    INT 21h
CALL DELAYWELL

    POP DX
    POP CX
    POP BX
    POP AX
RET
DRAWWELL ENDP


DRAWSQUARE PROC NEAR
PUSH AX
PUSH BX
PUSH CX
PUSH DX



	SUB YSQ,25
	SUB XSQ,25
	MOV CX,XSQ
	MOV DX,YSQ
	MOV BH,0
L1:
MOV CX,XSQ
MOV BL,0
L2:
mov al,0FH ;Pixel color
mov ah,0ch ;Draw Pixel Command
back: int 10h
inc cx
INC BL
cmp BL,50
jnz back
INC DX
INC BH
CMP BH,50
JNZ L1	
EXITSQUARE:
    POP DX
    POP CX
    POP BX
    POP AX
	RET
DRAWSQUARE ENDP

DRAWBLACKSQUARE PROC NEAR
PUSH AX
PUSH BX
PUSH CX
PUSH DX



	SUB YSQ,25
	SUB XSQ,25
	MOV CX,XSQ
	MOV DX,YSQ
	MOV BH,0
L1B:
MOV CX,XSQ
MOV BL,0
L2B:
mov al,00H ;Pixel color
mov ah,0ch ;Draw Pixel Command
backB: int 10h
inc cx
INC BL
cmp BL,163
jnz backB
INC DX
INC BH
CMP BH,20
JNZ L1B	

EXITSQUAREB:
    POP DX
    POP CX
    POP BX
    POP AX
	RET
DRAWBLACKSQUARE ENDP

ClearScreen proc Near
PUSH AX
PUSH BX
PUSH CX
PUSH DX
mov ax,0600h
mov bh,00fh
mov cx,0
mov dx,184FH
int 10h
    POP DX
    POP CX
    POP BX
    POP AX
	RET
ClearScreen ENDP

ClearScreenGAME proc Near
PUSH AX
PUSH BX
PUSH CX
PUSH DX
mov ax,0600h
mov bh,00fh
mov cx,0
mov dx,154FH
int 10h
    POP DX
    POP CX
    POP BX
    POP AX
	RET
ClearScreenGAME ENDP

PRINTDIGIT proc Near ;PRINT EVERY DIGIT ENTERED FROM THE PLAYER
PUSH AX
PUSH BX
PUSH CX
PUSH DX
PUSH SI
	MOV BH,0H
	mov ah,2
	mov dH,23
	MOV DL,13
	int 10h	
	
mov ah, 09H
mov dx, offset ENTERmes
int 21h

	MOV BH,0H
	mov ah,2
	MOV DL,9
	ADD DX,COUNTER
	mov dH,23
	int 10h	
LEA SI,ANSWER	
ADD SI,COUNTER
MOV DL,[SI]	
ADD DL,'0'
mov ah,2
int 21h
	POP SI
    POP DX
    POP CX
    POP BX
    POP AX
	RET
PRINTDIGIT ENDP

READ PROC NEAR ;READ THE PRESSED KEY ,STORE IT IN ANSWER AND CALL CHECKANSWER PROC IF THE ENTER IS PRESSED
	PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX
CMP AH,1CH
JZ CHECKANS
CMP COUNTER,3
JNZ CONTDATA
SUB COUNTER,3
CALL DRAWSTATUSBAR
CONTDATA:
LEA SI,ANSWER
ADD SI,COUNTER
SUB AL,'0'
MOV [SI],AL
CALL PRINTDIGIT
INC COUNTER
JMP EXITREAD
CHECKANS:
CALL CHECKANSWER
CALL DRAWSTATUSBAR
EXITREAD:
    POP DX
    POP CX
    POP BX
    POP AX
RET
READ ENDP

CHECKANSWER PROC NEAR ;GETS THE RIGHT ANSWER BY MULTIPLYING I*J AND COMPARES IT WITH THE STORED ANSWER IF EQUAL SET THE WIN FLAG TO 1
	PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX
     	
	MOV BH,0H
	mov ah,2
	mov dH,23
	MOV DL,13
	int 10h	
	
mov ah, 09H
mov dx, offset SPACEmes
int 21h	

MOV AL,i
mov bl,j
mul bl
LEA SI,ANSWER
MOV DX,AX

CMP COUNTER,3
JNZ CHECK2
MOV BL,100
MOV AL,[SI]
MUL BL
ADD NUMANSWER,AL
INC SI
MOV BL,10
MOV AL,[SI]
MUL BL
ADD NUMANSWER,AL
INC SI
MOV BL,1
MOV AL,[SI]
MUL BL
ADD NUMANSWER,AL

CMP NUMANSWER,DL
JNZ WRONG3
MOV BL,1
MOV WIN,BL
JMP EXITANSWER
WRONG3:
MOV BL,0
MOV WIN,BL
JMP EXITANSWER

CHECK2:
CMP COUNTER,2
JNZ CHECK1
MOV BL,10
MOV AL,[SI]
MUL BL
ADD NUMANSWER,AL
INC SI
MOV BL,1
MOV AL,[SI]
MUL BL
ADD NUMANSWER,AL

CMP NUMANSWER,DL
JNZ WRONG2
MOV BL,1
MOV WIN,BL
JMP EXITANSWER
WRONG2:
MOV BL,0
MOV WIN,BL
JMP EXITANSWER

CHECK1:
CMP COUNTER,1
JNZ EXITANSWER
MOV BL,1
MOV AL,[SI]
MUL BL
ADD NUMANSWER,AL


CMP NUMANSWER,DL
JNZ WRONG1
MOV BL,1
MOV WIN,BL
JMP EXITANSWER
WRONG1:
MOV BL,0
MOV WIN,BL
JMP EXITANSWER

EXITANSWER:
MOV BX,0
MOV COUNTER,BX
MOV NUMANSWER,BL
    POP DX
    POP CX
    POP BX
    POP AX
RET
CHECKANSWER ENDP
END MAIN

