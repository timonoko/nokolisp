	page ,132

;========================================================
;
; NOKOLISP josta tulee DXLISP, jos DXLISP on maaritelty.
; (DXLISP pyorii DX200-keskuksen sisalla)
; Tama kaantyy vain microsoftin MASM-assemblerilla
;
;========================================================


rapaseg segment para

ASSUME cs:rapaseg,DS:DATASEG,SS:STACKSEG,es:codeseg
sikaa label far
	mov	dx,es

	mov	cx,cs
	sub	cx,cs:[raparapa]
	mov	cs:[raparapa],cs

	mov	ax,cs:[rapacode]
	add	ax,cx
	mov	es,ax
	mov	cs:[rapacode],ax

	mov	ax,es:[cs_dataseg]
	add	ax,cx
	mov	ds,ax
	mov	es:[cs_dataseg],ax

	mov	ax,ss
	mov	es:[cs_stackseg],ax

	mov	[espsp],dx
	add	[nodtyp],cx
	add	[carlow],cx
	add	[carhig],cx
	add	[cdrlow],cx
	add	[cdrhig],cx
	add	[lastmem],cx
	add	[ds_rapaseg],cx
	add	[ds_exeseg],cx

	push	es
	mov	ax,offset continue_in
	push	ax
	db 0cbh ; RETF  ja vittu, kun se pitaa aina kirjoittaa nain

	rapacode dw codeseg
	raparapa dw rapaseg	

rapaseg ends

exeseg segment para
; exe-filen headeri:
	db	4DH,5AH	; 0
	dw	1ffH	; 2 
	dw	0	; 4 size-of-exe in 512 - byte parag
	dw	0	; 6  
	dw	2	; 8  header size = 32 bytes
	dw	0	; a
	dw	0ffffh	; c
	dw	0	; e  SS
	dw	0	; 10 SP 
	dw	0	; 12
	dw	0	; 14
	dw	0	; 16
	dw	1eH	; 18
	dw	1	; 1a

	dw	0	; 1c
	dw	0	; 1e
	
exeseg ends

; +20H



STACKSEG SEGMENT PARA STACK 'STACK'
	Db 20000 DUP(?)
	TOPOFSTACK DW 0 ; Pinon ylivuotopaikka
STACKSEG ENDS


DATASEG	SEGMENT PARA 'DATA'

	DB 100H	DUP(?) ; pinon ylivuototilaa

; Yks lisp-solu on viiden byten mittainen, jokainen byte omassa
; segmentissaan. Segmenttien paikka ja koko lasketaan vasta
; ohjelmaa kaynnistettaessa:

	nodtyp dw 0   ; solun tyyppi
	carlow dw 0   ; CAR
	carhig dw 0   ;
	cdrlow dw 0   ; CDR
	cdrhig dw 0   ;

; Varattuja osoitteita:
	nil	equ	0
	t	equ	1
	QUOTE	EQU	2
        ; 2 - 0FFH tyhjaa
	; 100H - 200H pienet numerot
	ZERONUM	EQU	3	; nolla
	FSTNODE	EQU	300H	; Pienin Cons-solmu 
	LASTNODE DW	0FFFEH  ; Ylin solmu


; Tyyppikentan arvoja:
	NUMBER	EQU	3
	STRING	EQU	4
	SYMBOL	EQU	5
	FSYMBO	EQU	6
	SUBRU	EQU	7
	LIST	EQU	0FH
	GARBA	EQU	10H
	CIRCUL	EQU	20H

; Muuttujia:

	OBLIST 		DW 	NIL ; Oblistin juuri

	FREELIST	DW	nil ; eka vapaa solu

	ARGSTA		DW	NIL  ; sisakkaisten lausekkeiden "hannat" -
				     ;   eli argumentit

	ENVIRO		DW	NIL  ; Muuttujien vanhoja arvoja -
                                     ; (((Name N2 ..) Value V2 ..)((Name

	TRACE		DW	NIL  ; Eval trace
	TRON		DW	0FFH ; Trace paalla

	MACDAT		DW	0 ; kaannettyjen datat

	theseclauses	dw	0 ; Kaantajan ihmeita?

	RDATOM		DB	0 ; Readerin lippuja

	espsp		dw	0 ; Dosin antama SP-arvo?

	lastmem		dw	0

	hex		db	0 ; Tulostus hexamuodossa-lippu

	FSYMBC		dw	FSYMBS ; Eka vapaa kaannetty muuttuja paikka
	FSYMBS		dw	3000 dup (?) ; Kaannetyt muuttujat

DATASEG  ENDS

  
CODESEG SEGMENT para  'CODE'
ASSUME cs:CODESEG,DS:DATASEG,SS:STACKSEG


;============================== DXLISPIA =================================
IFDEF DXLISP 
	DW	16DBH
	CALL	LISP
	DB	0CBH
	DB	0FFH
	DW	OFFSET END_OFFSET_OF_CODE
	DW	0FFFFH
	DW	CODESEG

	PIDSTRING: DB '@(#)PID: NOKOLISP.ASM 1.0 12/12/86'
ORG	38H
	DB	0
	DB	0FFH
	DW	0 ; END_OFFSET_OF_DATA
	DW	0FFFFH
	DW	DATASEG
END_OFFSET_OF_CODE: DW 0FFH

NEW_DX_SP EQU 0A000H

DATASEG segment
	dx_exit db 0
	PROMPTI DB 'NL>$',0
DATASEG	ENDS

JUMP	MACRO 	SEG,OFFS
	DB	0EAH
	DW	OFFS
	DW	SEG
	ENDM


CLUCIF_CODE equ 0F000H 

READ_COMMAND_LINE label far
	JUMP CLUCIF_CODE,076H
NEXT_CHAR LABEL FAR
  	JUMP CLUCIF_CODE,07BH
CHAR_OUT label far
  	JUMP CLUCIF_CODE,0C1H
CHAR_IN label far
	JUMP CLUCIF_CODE,0B7H
TI label far
	JUMP CLUCIF_CODE,0CBH


DX_COMMAND_LINE:
	MOV	BP,SP
	MOV	AX,DATASEG
	PUSH	AX
	MOV	AX,OFFSET PROMPTI
	PUSH	AX
	CALL	READ_COMMAND_LINE
	RET

dataseg segment
	buffersegment dw 0
	bufferpointer dw 0
dataseg ends

READC:	PUSH	AX
	PUSH	BX
	PUSH	CX
	mov	dl,[nxtch]
	push	dx
	mov	ax,[inchn]
	cmp	al,0
	jz	conin
	mov	es,[buffersegment]
	mov	bx,[bufferpointer]
	mov	al,es:[bx]
	inc	bx
	mov	[bufferpointer],bx
	jmp	eeee
CONIN:  MOV	BP,SP
	CALL	TI
eeee:	mov	[nxtch],al
	POP	DX
	mov	al,[echo]
	cmp	al,0
	jz	eiechoa
	call	printc
eiechoa:POP	CX
	POP	BX
	POP	AX
	RET


PRINTC:	PUSH	AX
	PUSH	BX
	PUSH	CX
	PUSH	DX
	mov	bx,offset tabs
	inc	byte ptr [bx]
	cmp	dl,13
	jnz	rivisi
	mov	byte ptr [bx],-1
rivisi:	mov	ax,[outchn]
	cmp	al,0
	jz	conout
	mov	es,[buffersegment]
	mov	bx,[bufferpointer]
	mov	es:[bx],dl
	inc	bx
	mov	[bufferpointer],bx
	jmp	prce
conout:	MOV	BP,SP
	PUSH	DX
	CALL	CHAR_OUT
prce:	POP	DX
	POP	CX
	POP	BX
	POP	AX
	RET

ELSE;=============================== NOKOLISPIA TAAS ========================

msdos	macro	num
	mov	ah,num
	int	21h
	endm

ENDIF;=====================================================================

loppu:
 NXTSBR = offset loppu

; Lisp$subru - macrolla maaritellaan alkeisfunktioita, ne muodostavat
; NXTSBR - muuttujien avulla ketjun, joka tulkitaan initialisointi-
; vaiheessa.
LISP$SUBRU MACRO NameOfSubru,howtocomp,ARG1
	CODEADDR = OFFSET $
dataseg segment
	TEMPADDR = OFFSET $
	DW NXTSBR
	NXTSBR = TEMPADDR
	dw CODEADDR
	dw howtocomp
  	DB NameOfSubru,0
dataseg ends
	IFNB <ARG1>
	CALL	ARG1
	ENDIF
	ENDM   

IFDEF DXLISP ;=========================================================

lisp$subru 'dxbuffer',cnumarg,numarg
	mov	[buffersegment],ax
	mov	[bufferpointer],0
	jmp	tret

lisp$subru 'clucif_code',cantcomp
	mov	ax,clucif_code
	jmp	maknum

LISP$SUBRU 'nquit',cantcomp
	mov	[dx_exit],0FFh
	MOV	AX,[SP_JEMMA]
	MOV	SP,AX
	RET

LISP$SUBRU 'readcc',cnoarg
	mov	bp,sp
	call	char_in
	mov	ah,0
	jmp	maknum

ENDIF ;====================================================================

dataseg segment
     stackmark dw 0
     throwtag dw 0
dataseg ends

; Taman avulla tehdaan CATCH/THROW kehyksia
FRAME	MACRO	EXIT_ADDRESS
	mov	di,offset EXIT_ADDRESS
	push	DI
	PUSH	[STACKMARK]
	MOV	[STACKMARK],SP
	ENDM

; ================= 8080 - makroja: ===============================

RZ	MACRO
	JNZ	$+3
	RET
	ENDM

RNZ	MACRO
	JZ	$+3
	RET
	ENDM

RC	MACRO
	JNC	$+3
	RET
	ENDM

RNC	MACRO
	JC	$+3
	RET
	ENDM

JMPN	MACRO TEST,ADDR
	J&TEST	$+5
	JMP	ADDR
	nop
	ENDM

; === Lisp-primitiivien primitiiveja: =================================

dataseg segment
	handle_name		DB 'NokoLisp'
	EMS_ON			db 0
	page_frame_segment	dw 0
	emm_handle		DW 0
	logical_page_number	dw 0FFFFH
dataseg ends

ems_reset:
	mov	[logical_page_number],0FFFFH
	ret
mappage:
	MOV	[logical_page_number],AX
	PUSH	DX
	PUSH	BX
	MOV	DX,[emm_handle]		; load EMM handle
	MOV	BX,AX			; load logical page number
	MOV	AL,0			; load physical page number
	MOV	AH,44h			; load function code
	INT	67h			; call the memory manager
	OR	AH,AH			 ; check EMM status
	JMPN	Z,emm_err_handler	 ; jump to error handler on error
	POP	BX
	POP	DX
	RET

EMSADDR:
	PUSH	AX
	PUSH	CX
	Mov	AX,DI
	MOV	CL,11
	ROR	AX,CL
	AND	AX,1FH
	CMP	AX,[logical_page_number]
	JZ	EMSADR2
	CALL	MAPpage
EMSADR2:MOV	CL,3
	ROL	DI,CL
	AND	DI,3FF8H
	MOV	ES,[page_frame_segment]
	POP	CX
	POP	AX
	RET

;==============================
EMSMAC	MACRO OFSET,REG,FROM,
	LOCAL	ei_ems
	IFDEF   FROM
	MOV     DI,FROM
	ENDIF
	CMP	[EMS_ON],0
	JZ	ei_ems
	IFDEF   FROM
	CALL	EMSADDR
	ENDIF
	MOV	REG&X,ES:[DI+OFSET]
	RET
ei_ems:
	ENDM	

newcrmac2 macro CR,OFSET,TO,FR
	local	ohi
	jmp	short ohi
NEW&CR&TO&FR:
	EMSMAC	OFSET,TO,FR
	mov	ES,[CR&LOW]
	mov     TO&L,ES:[DI]
	mov	ES,[CR&HIG]
	mov	TO&H,ES:[DI]
	RET
ohi:
	endm

newcrmac macro CR,OFSET,TO,FR
	IFNDEF NEW&CR&TO&FR
	newcrmac2  CR,OFSET,TO,FR
	ELSEIF NEW&CR&TO&FR GT OFFSET $
	newcrmac2  CR,OFSET,TO,FR
	ENDIF
	CALL	NEW&CR&TO&FR
endm

CAR	MACRO   REG,FROM
	newcrmac CAR,4,REG,FROM	
	ENDM

CDR	MACRO   REG,FROM
	newcrmac CDR,6,REG,FROM	
	ENDM
	   
TYPEOF	MACRO  REG,FROM
	LOCAL	ei_ems,juu_ems
	IFDEF   FROM
	MOV     DI,FROM
	ENDIF
	CMP	[EMS_ON],0
	JZ	ei_ems
	IFDEF   FROM
	CALL	EMSADDR
	ENDIF
	jmp	short juu_ems
ei_ems:
	mov	ES,[NODTYP]
juu_ems:
	MOV	REG,ES:[DI]
	ENDM

NEWRPL	MACRO   CR,REG,OFS
	local	ei_ems,juu_ems
	CMP	[EMS_ON],0
	JZ	ei_ems
	CALL	EMSADDR
	MOV	ES:[DI+OFS],REG&X
	JMP	SHORT juu_ems
ei_ems:	mov	ES,[CR&LOW]
	mov     ES:[DI],REG&L
	mov	ES,[CR&HIG]
	mov	ES:[DI],REG&H
juu_ems:
	ENDM

RPLACA	MACRO   LISTA,REG
	MOV	DI,LISTA
	NEWRPL	CAR,REG,4
	ENDM

RPLACD	MACRO   LISTA,REG
	MOV	DI,LISTA
	NEWRPL	CDR,REG,6
	ENDM

SETTYPE	MACRO   OLIO,VAL
	LOCAL	ei_ems,juu_ems
	MOV     DI,OLIO
	MOV	DL,VAL
	CMP	[EMS_ON],0
	JZ	ei_ems
	CALL	EMSADDR
	jmp	short juu_ems
ei_ems:
	mov	ES,[NODTYP]
juu_ems:
	MOV     ES:[DI],DL
	ENDM

lisp$subru 'use-EMS',cnoarg
	cmp	[EMS_ON],0
	JMPN	Z,TRET
	XOR	AX,AX
	MOV	ES,AX
	MOV	AX, ES:19CH 
	OR	AX, ES:19EH
	JMPN	NZ,NILRET
	MOV	AH,40h			 ; load function code
	INT	67h			 ; call the memory manager
	OR	AH,AH			 ; check EMM status
	JMPN	Z,NILRET		 ; jump to error handler on error

	MOV	AH,41h			 ; load function code
	INT	67h			 ; call the memory manager
	OR	AH,AH			 ; check EMM status
	JMPN	Z,NILRET		 ; jump to error handler on error
	MOV	[page_frame_segment],BX	   ; save page frame address

	MOV	BX,34			; load number of pages
	MOV	AH,43h			 ; load function code
	INT	67h			 ; call the memory manager
	OR	AH,AH			 ; check EMM status
	JMPN	Z,NILRET		 ; jump to error handler on error
	MOV	emm_handle,DX		 ; save EMM handle

	mov	CX,-1
SIIRTO:	INC	CX
	PUSH	CX
	Mov	EMS_ON,0
	TYPEOF	dl,CX
	CAR	A,CX
	CDR	B,CX
	MOV	EMS_ON,0FFH
	SETTYPE CX,dl
	RPLACA	CX,A
	RPLACD	CX,B
	POP	CX
	CMP	CX,[LASTNODE]
	JMPN	Z,SIIRTO

	MOV	[LASTNODE],0FFFEH

	MOV	AX,[NODTYP]
	MOV	[LASTMEM],AX

	MOV	SI,Offset handle_name	 ; DS:SI points to handle_name
	MOV	DX,emm_handle		 ; specify EMM handle
	MOV	AX,5301h		 ; load function code
	INT	67h			 ; call the memory manager
	MOV	AH,4Bh			      ; load function code
	INT	67h			      ; call the memory manger
	OR	AH,AH			      ; check EMM status
	JMPN	Z,NILRET		 ; jump to error handler on error
	MOV	AX,BX	   
	JMP	MAKNUM

CAR$CDR	MACRO	RCAR,RCDR,R2
	CAR	RCAR,R2
	CDR     RCDR
	ENDM

J$ATOM  MACRO  REG,TO
	CMP	REG,FSTNODE
	JMPN	NC,TO
	TYPEOF  DL,REG
	CMP	DL,LIST
	JMPN	Z,TO
	ENDM

R$ATOM	MACRO	REG
	CMP	REG,FSTNODE
	RC
	TYPEOF  DL,REG
	CMP	DL,LIST
	RNZ
	ENDM

J$NIL	MACRO	REG,ADDR
	CMP	REG,0
	Jmpn	nZ,ADDR
	ENDM

R$NIL	MACRO	REG
	CMP	REG,0
	RZ
	ENDM


JN$NIL	MACRO	REG,ADDR
	CMP	REG,0
	JmpN	Z,ADDR
	ENDM

; Suorita NZ,Z etc-testaus palauta T tai NIL
TEST$RESU	macro	test
	mov	ax,1
	j&test	$+3
	dec	ax
	ret
	endm

; Tulosta BX-osoittama merkkijono
PRINTS:	MOV	DL,[BX]
	CMP	DL,0
	RZ
	CALL	PRINTC
	INC	BX
	JMP	PRINTS

; priths 'paata saarkee'
PRITHS	MACRO	STR
dataseg segment
	tempaddr =  $
	DB STR,0
dataseg ends
	push	bx
	mov	bx,offset tempaddr
	CALL	PRINTS
	pop	bx
	ENDM 

NILRET:	MOV 	AX,NIL
	RET
TRET:	MOV	AX,T
	RET

WHEN    MACRO WHAT,ADDR
	CMP	DL,WHAT
	JMPN	NZ,ADDR
	ENDM

; =========== Lisp Error =============================
dataseg segment
	erheen_syy dw 0
dataseg ends
LISP$ERROR macro str
	dataseg segment
	   tempaddr = $
	   db str,0
	dataseg ends
	mov	[throwtag],nil
	MOV	BX,OFFSET tempaddr
	mov	[erheen_syy],bx
	call	ERHE
	ret
endm
emm_err_handler:
	LISP$ERROR 'FATAL EMS-ERROR'

;=====================================================================
;=== Compileri macrot

; Generoi 'CALL'

COCALL	MACRO	what,str
	local	skp
	mov	ax,offset what
	call	ccall 
	mov	dl,[codebug]
	cmp	dl,0
	jz	skp
	priths '	; CALL '
	priths	str
skp:
	endm

; Mika tahansa kama, paitsi relatiiviset jutut, voidaa kaantaa
; COMPIL-makron avulla.
COMPIL	MACRO	NumberOfBytes,Instruction,HowToWrite
	local	addr
	local	ohi
DATASEG	SEGMENT
	STRA =  $
	DB HowToWrite,0
DATASEG ENDS
	push	cx
	PUSH	BX
	IF	NumberOfBytes 
	mov	cx,NumberOfBytes
	ELSE
	MOV	CX,offset (OHI-ADDR)
	ENDif
	mov	si,offset addr
	MOV	BX,offset STRA
	call	maccod
	POP	BX
	pop	cx
	jmp	ohi
addr:	Instruction
ohi:
	ENDM

comptext macro str
	local 	ohi
	mov	dl,[codebug]
	cmp	dl,0
	jz	ohi
	priths	str
ohi:
	endm


;========================================================================
;============== ROSKIEN KERUU
;=== "Symboli, jonka arvo NIL, ja johon kukaan ei viittaa, poistetaan."

; datojen merkkaus:
MARK:	cmp	ax,fstnode ; 
	rc
	cmp	ax,[lastnode]
	rnc
	TYPEOF	DL,AX
	TEST	DL,GARBA
	RZ
	AND	DL,NOT GARBA
	MOV	ES:[DI],DL
	WHEN	LIST,MARKLIST
	WHEN	SYMBOL,MARKTAIL
	WHEN	FSYMBO,MARKTAIL
	WHEN	STRING,MARKTAIL
	RET
MARKTAIL:
	CDR	A
	JMP	MARK	
MARKLIST:
	CDR	B
	PUSH	BX
	CAR	A
	CALL	MARK
	POP	aX
	JMP	MARK

; Roskienkeruu:
GARCOL:

IFNDEF	DXLISP ; IBM:ssa tulee tammonen piippi aina roskienkeruun alussa
	mov	dx,5
	in	al,61h
	and	al,11111110b
peeppi:	or	al,10b
	out	61h,al
	mov	cx,100
fh:	loop	fh
	and	al,11111101b
	out	61h,al
	mov	cx,100
sh:	loop	sh
	dec	dx
	jnz	peeppi
ENDIF

; Ensiks kaikki roskiksi
	mov	ax,FSTNODE
EMPTY:	TYPEOF	DL,AX
	OR	DL,GARBA
	MOV	ES:[DI],dl
	INC	ax
	cmp	ax,[lastnode]
	jnz	empty

; Sitten aletaan etsia datoja
	MOV	AX,[OBLIST]

MARKO1:	J$NIL	AX,MOBDON
	TYPEOF	DL,AX
	AND	DL,NOT GARBA
	MOV	ES:[DI],DL
	CDR	A
	JMP	MARKO1

MOBDON:	MOV	AX,[OBLIST]
MARKO2:	J$NIL	AX,MOBDO2
	CAR$CDR	A,B,AX
	PUSH	BX
	CAR	A,AX
	JN$NIL	AX,DOMARK
MARKO3:	POP	AX
	JMP	MARKO2
DOMARK:
	TYPEOF	DL
	AND	DL,NOT GARBA
	MOV	ES:[DI],DL
	CDR	B
	PUSH	BX
	cmp	dl,fsymbo
	jnz	nofsym
	mov	bx,ax
	mov	ax,[bX]
nofsym:	CALL	MARK
	POP	AX
	CALL	MARK
	JMP	MARKO3
MOBDO2:
	MOV	Bp,SP
MARKSP:

IFDEF DXLISP
	CMP	Bp,NEW_DX_SP
ELSE
	CMP	Bp,offset topofstack
ENDIF

	JZ	EMRKSP
MARKS2:	MOV     AX,[bp]
	PUSH	Bp
	CALL	MARK
	POP	Bp
	INC	Bp
	INC	Bp
	JMP	MARKSP
EMRKSP:
	MOV	AX,[ARGSTA]
	CALL	MARK
	MOV	AX,[ENVIRO]
	CALL	MARK
	MOV	AX,[TRACE]
	CALL	MARK
	MOV	AX,[MACDAT]
	CALL	MARK
	mov	ax,[theseclauses]
	call	mark
	MOV	AX,[OBLIST]
	CALL	FREEOBL
	MOV	[OBLIST],AX
	mov	cx,0
	mov	bx,nil
	MOV	aX,FSTNODE
VARMA:	TYPEOF	DL,AX
	TEST	DL,GARBA
	JZ	VARMA2
	rplacd	ax,b
	mov	bx,ax
	inc	cx
varma2:	inc	ax
	cmp	ax,[lastnode]
	jnz	varma
	mov	[freelist],bx
	cmp	cx,1000
	jc	outofm1
	ret

lisp$subru 'more-memory',cantcomp
	call	outofm1
	mov	ax,[lastmem]
	jmp	maknum


OUTOFM1:
	mov	ax,[lastnode] ; esim 16
	test	ax,8000H
	jmpn	z,outofmem
	shr	ax,1
	shr	ax,1
	mov	bx,ax ; 4
	shr	ax,1
	shr	ax,1 ; 1
	add	bx,ax ; 5
	add	bx,[lastmem]
	push	bx
	mov	ax,[espsp]
	mov	es,ax
	sub	bx,ax
	msdos	4ah
	jnc	con2
	pop	bx
	cmp	ax,8
	jmpn	nz,outofmem
	jmp	stbler

con2:	pop	[lastmem]
	mov	ax,[lastnode] ; esim 16
	shr	ax,1	; 8 
	shr	ax,1	; 4
	cld
	mov	bx,[cdrhig] ; source
	call	sipa
	mov	[cdrhig],bx
	shr	ax,1	; 2
	mov	dx,ax
	shr	ax,1	; 1
	add	ax,dx	; 3
	mov	bx,[cdrlow] ; source
	call	sipa
	mov	[cdrlow],bx
	mov	ax,dx	; 2
	mov	bx,[carhig] ; source
	call	sipa
	mov	[carhig],bx
	shr	ax,1	; 1
	mov	bx,[carlow] ; source
	call	sipa
	mov	[carlow],bx
	shl	[lastnode],1
	ret

sipa:	mov	cx,[lastnode]  ; counteri
	mov	ds,bx
	add	bx,ax
	mov	es,bx
	mov	di,0
	mov	si,0
	rep	movsb
	mov	ds,cs:[cs_dataseg]
	ret

outofmem:
	priths	<13,10,'More Memory?'>
	MOV	AX,nil
	ret


LISP$SUBRU	'reclaim',cnoarg
RECLAIM:CALL	GARCOL
	MOV	AX,CX
	jmp	maknum

; Tassa putsataan OBLISTi roskista
FREEOBL:R$NIL	AX
	CAR$CDR	B,C,AX
	TYPEOF	DL,BX
	TEST	DL,GARBA
	JZ	NOSKIP
	MOV     AX,CX
	JMP	FREEOBL
NOSKIP:	PUSH	AX
	MOV 	AX,CX
	CALL	FREEOBL
	MOV	BX,AX
	POP     AX
	RPLACD	AX,B
	RET

;================= PerusCONSIT =====================================

ANYCONS:
	MOV	CX,[freelist]
	J$NIL	CX,TOGARCOL
	CDR	d,CX
	MOV	[FREELIST],DX
CONS2:	RPLACA	CX,A
	RPLACD	CX,B
	MOV	AX,CX
	RET
TOGARCOL:
	PUSH	AX
	PUSH	BX
	CALL	GARCOL
	POP	BX
	POP	AX
	jmp	anycons

LISP$SUBRU 'cons',ctwoarg,twoarg
CONS:	CALL	ANYCONS
 	SETTYPE	AX,LIST
	RET

MAKNUM:	cmp	ax,(FSTNODE-ZERONUM)
	jc	smallnum
	MOV	BX,0
	CALL	ANYCONS
	SETTYPE AX,NUMBER
	RET
smallnum:
	ADD	AX,ZERONUM
	ret

MAKSBR:	CALL	ANYCONS
 	SETTYPE AX,SUBRU
	RET

; AX~value,BX~string
MAKNAM:	PUSH	AX
 	CALL	MAKSTR
	MOV	BX,AX
	POP	AX
	CALL	ANYCONS
	SETTYPE	AX,SYMBOL
	RET


MAKSTR: ;BX=JONO+0
 	MOV	DX,[BX]
	CMP	DL,0
	JMPN	NZ,NILRET
	INC	BX
	CMP	DH,0
	JNZ	EEMAK2
	DEC	BX
EEMAK2:	INC	BX
	PUSH	DX
	CALL	MAKSTR
	MOV	BX,AX
	POP	AX
	CALL	ANYCONS
	SETTYPE	AX,STRING
	RET

;========================== Readerit ===============================


NAP	MACRO	L1,L2
	LOCAL	skp
	CMP	DL,L1
	JC	SKP
	CMP	DL,L2+1
	CMC
SKP:
	ENDM

NUMP:	NAP	'0','9'	
	RET
NUMALF:	CALL	NUMP
	JNC	TRUE
	NAP	'A','Z'
	JNC	TRUE
UALFP:	NAP	'a','z'
	RET
UPCHA:	CALL	UALFP
	JC	UPOK
	SUB	DL,'a'-'A'
UPOK:	RET

TRUE:	CLC
	RET
FALSE:	STC
	RET

DATASEG SEGMENT
	EOF$ST  	db 'END-OF-FILE',0
DATASEG ENDS

EOF$:	MOV	BX,OFFSET EOF$ST
	JMP	RETUID

LISP$SUBRU 'read',CNOARG
READ:	mov	dl,0
	MOV	[TABS],dl
	CALL	READC
	WHEN	1AH,EOF$
	CMP	DL,21H
	JC	READ
	cmp	dl,';'
	jnz	notcom
	mov	dh,[nxtch]
	cmp	dh,';'
	jnz	notcom
comment2:call	readc
	cmp	dl,13
	jnz	comment2
	jmp	read
notcom:	MOV	[RDATOM],DL
	WHEN	'(',RLIST
	WHEN	')',NILRET
	WHEN	27H,RQUOT
	cmp	dl,'.'
	jnz	itemr1
	mov	al,nxtch
	cmp	al,21h
	jmpn	nc,tret
itemr1:	MOV	BX,OFFSET ITEMB
ITEMR:	MOV	[BX],DL
	INC	BX
	MOV	DL,[NXTCH]
	CMP	DL,21H
	JC	ITEMC
	WHEN	'(',ITEMC
	WHEN	')',ITEMC
	CALL	READC
	JMP	ITEMR

	; dos -rivista ASCIIZ-RIVIKSI
	
RETUID:	MOV	DI,offset ITEMB ; BX :=string+0
RETUIL:	MOV	DL,[BX]
	MOV	[DI],DL
	CMP	DL,0
	JZ	ITEMC2
	INC	DI
	INC	BX
	JMP	RETUIL
	

ITEMC:	MOV	DL,0
	MOV	[BX],DL
ITEMC2:	MOV	BX,offset itemb
	MOV	DL,[bx]
	when	0,nilret
	when	'$',rdhex
	when	'-',RNEG
	CALL	RNUM
	JMPN	NC,TRALFA
	JMP	MAKNUM
rdhex:	inc	bx
	mov	dl,[bx]
	cmp	dl,0
	jmpn	nz,tralfa
	mov	ax,0
rdhex2:	mov	cx,16
	mov	dl,[bx]
	inc	bx
	cmp	dl,0
	jmpn	nz,maknum
	nap	'0','9'
	jc	alfa
	sub	dl,'0'
	jmp	numbi
alfa:	nap	'A','F'
	jmpn	nc,tralfa
	sub	dl,'A'-10
numbi:	push	dx
	mul	cx
	pop	dx
	add	ax,dx
	jmp	rdhex2

RNUM:	MOV	AX,0
	MOV	CX,10
	MOV	DH,0
RNUM2:	MOV	DL,[BX]
	INC	BX
	CMP	DL,0
	RZ
	NAP	'0','9'
	RC
	SUB	DL,'0'
	push	dx
	MUL	CX
	cmp	dx,0 ; lisaa
	pop	dx
	jmpn	z,false ; lisaa
	ADD	AX,Dx
	cmp	ax,8000h
	jmpn	c,false
	JMP	RNUM2

purge	nap

RNEG:	INC	BX
	MOV	DL,[BX]
	CMP	DL,0
	JMPN	NZ,TRALFA
	CALL	RNUM
	JC	TRALFA
	NEG	AX
	JMP	MAKNUM
	
RCONS:	CALL	RLIST
	JMP	SCAR

RLIST:	CALL	READ
	MOV	DL,[RDATOM]
	CMP	DL,0
	JZ	UUSNIL	
	mov	dl,0
	MOV	[RDATOM],DL
	R$NIL	AX
UUSNIL:	PUSH	AX
RDCONS:	MOV	DL,[NXTCH]
	CMP	DL,21H
	JNC	RDCON2
	CALL	READC
	JMP	RDCONS
RDCON2:	PUSH	DX
	CALL	RLIST
	POP	DX
	CMP	DL,'.'
	JNZ 	EICADR
	CALL	CADR
EICADR:	MOV	BX,AX
	POP	AX
	JMP	CONS
RQUOT:	CALL	READ
	MOV	BX,NIL
	CALL	CONS
	MOV	BX,AX
	MOV	AX,QUOTE
	JMP	CONS	

dataseg	segment
	hashtable dw 100h dup (0)
	thishash dw 0
dataseg ends

makhash:mov	bx,offset itemb
	mov	ax,0
makha2:	mov	dl,[bx]
	cmp	dl,0
	rz
	inc	bx
	add	al,dl
	jmp	makha2

TRALFA:	call	makhash
	add	ax,ax
	add	ax,offset hashtable
	mov	[thishash],ax
	mov	bx,ax
	mov	ax,[bx]
	cmp	ax,fstnode
	jc	ralfa1
	typeof	dl,ax
	cmp	dl,symbol
	jz	ralfa0
	cmp	dl,fsymbo
	jnz	ralfa1
ralfa0:	push	ax
	call	alfmat
	pop	ax
	jz	nfoun2
ralfa1:	MOV	AX,[OBLIST]
RALF2:	J$NIL	AX,NEWNAM
	PUSH	AX
	CAR	A,AX
	CALL	ALFMAT
	JZ	NFOUN
	POP	AX
	CDR	A,AX
	JMP	RALF2
NFOUN:	POP	AX	
	CAR	A,AX
	mov	bx,[thishash]
	mov	[bx],ax
nfoun2:	cmp	ax,[quotes]
	jz	rdquote
	cmp	ax,[trues]
	rnz
	mov	ax,t
	ret
rdquote:
	mov	ax,quote
	RET

NEWNAM:	MOV	BX,OFFSET ITEMB
	MOV	AX,NIL
oblnam:	CALL	MAKNAM
	mov	bx,[thishash]
	mov	[bx],ax
PUSHOB:	PUSH	ax
	mov	bx,nil
	call	cons
	MOV	bX,[OBLIST]
	RPLACD	ax,b
	MOV	[OBLIST],AX
	POP	AX
	RET


ALFMAT:	cdr	a,ax
	MOV	BX,OFFSET ITEMB
RNAM2:
	CAR	C,AX
	CMP	[BX],CL
	Rnz
	Inc	Bx
	Cmp	[BX],CH
	RNZ
	CMP	ch,0
	RZ
	INC	BX
	CDR	A,AX
	JN$NIL	AX,RNAM2	
	CMP	[bx],AL
	RET	
		
dataseg segment
	atulos dw 0
dataseg ends

lisp$subru 'apropos',cantcomp,strarg
	mov	word ptr [atulos],nil
	mov	bx,[oblist]
APRLOO:	j$NIL	bx,adone
	car$cdr	c,b,bx
	push	ax
	push	bx
	push	cx
	cdr	c,cx
smore:	cmp	ax,cx ; 
	jz	pushthis
	cmp	ax,nil
	jz	pushthis
	cmp	cx,nil
	jz	einaa
	car$cdr	b,a,ax
	car$cdr	d,c,cx
	cmp	bx,dx
	jz	smore
	cmp	bl,dl
	jnz	einaa
	cmp	bh,0
	jnz	einaa
pushthis:
	pop	ax
	mov	bx,[atulos]
	call	cons
	mov	[atulos],ax
	push	cx
einaa:	pop	cx
	pop	bx
	pop	ax
	jmp	aprloo
Adone:	mov	ax,[atulos]
	ret
	
LISP$SUBRU 'print',CONEARG,EVALARG
	PUSH	AX
	CALL	PRINT
	POP	AX
	RET

PRINT:	J$ATOM	AX,PATOM
	CAR	B,AX
	CMP	BX,QUOTE
	JMPN	NZ,PQUOT2
PRLIS:	MOV	DL,'('
	CALL	PRINTC
PRLIS2:	PUSH	AX
	CAR	A,AX
	CALL	PRINT
	POP	AX
	CDR	A,AX
	J$ATOM	AX,PDOT
	MOV	DL,20H
	CALL	PRINTC
	JMP	PRLIS2
PDOT:	J$NIL	AX,EPRL
	PUSH	AX
	PRITHS	' . '
	POP	AX
	CALL	PATOM
EPRL:	MOV	DL,')'
	JMP	PRINTC


PATOM:	TYPEOF	DL,AX
	WHEN	symbol,PNAME
	when	fsymbo,pname
	WHEN	SUBRU,PSUBR
	WHEN	NUMBER,PNUMB
	WHEN	STRING,PRSTR2
	WHEN	T,PT
	WHEN	QUOTE,PQUOT
	WHEN	NIL,PNIL
	MOV	DH,0
	PUSH	DX
	PUSH	AX
	PRITHS	'?'
	POP	AX
	CALL	PNUM
	MOV	DL,':'
	CALL	PRINTC
	POP	AX
	JMP	PNUM


PNIL:	PRITHS	'()'
	RET
PT:	PRITHS	't'
	RET
PNAME:	CDR	A,AX
	CALL	PRSTRI
	RET
PQUOT:	PRITHS 	'quote'
	RET
PQUOT2:	PUSH	AX
	PRITHS 27H
	POP	AX
	CDR	A,AX
	CAR	A,AX
	JmP	PRINT
PSUBR:	PUSH	AX
	PRITHS	'  (subru: eval='
	POP	AX
	CAR$cdr	A,b,AX
	push	bx
	call	hexword
	priths ', compile='
	pop	ax
	call	hexword
	priths  ')'
	ret
PNUMB:	CAR	A,AX
	JMP	PNUM2

PRSTRI:	R$NIL	AX
	CAR$CDR	D,A,AX
	CALL	PRINTC
	CMP	DH,0
	RZ
	MOV	DL,DH
	CALL	PRINTC
	JMP	PRSTRI

PRSTR2:	MOV	DL,'"'
	CALL	PRINTC
	CALL	PRSTRI
	MOV	DL,'"'
	CALL	PRINTC
	RET

dataseg segment
	zeroflag db 0
dataseg ends

lisp$subru 'hex',conearg,evalarg
	mov	hex,al
	ret
pnum:
pnum2:	mov	dl,[hex]
	cmp	dl,0
	jmpn	z,hexword
	CALL	EXPLNU
	MOV	BX, OFFSET ITEMB
	CALL	PRINTS
	RET

EXPLNU:	MOV	DI,OFFSET ITEMB
	CALL	EXPLN2
	MOV	DL,0
expnCH:	MOV	[DI],DL
	INC	Di
	RET

pelnol:	MOV	DL,'0'
	JMP	EXPNCH

EXPLN2:	cmp	ax,0
	jz	pelnol
	mov	dl,0
	mov	[zeroflag],dl
	TEST	AH,80H
	JZ	PNUM3
	MOV	DL,'-'
	CALL	EXPNCH
	NEG	AX
PNUM3:	MOV	BX,10000
	MOV	CX,10
PNUM4:	CMP	BX,0
	RZ
	CMP	AX,BX
	JNC	JOOJOO
	PUSH	AX
	mov	dl,[zeroflag]
	cmp	dl,0
	jz	pnum5
	MOV	DL,'0'
	CALL	EXPNCH
	JMP	PNUM5
JOOJOO:	mov	dx,0
	DIV	BX
	PUSH	DX
	ADD	AX,'0'
	MOV	DL,AL
	mov	[zeroflag],dl
	CALL	EXPNCH
PNUM5:	MOV	AX,BX
	MOV	dx,0
	DIV	Cx
	MOV	BX,AX
	POP	AX
	JMP	PNUM4

LISP$SUBRU 'cr',cNOARG
CRLF:	mov	dl,13
	call	printc
	mov	dl,10
	CALL	printc
	JMP	NILRET

TOODEEP:
	mov	ax,nil
	mov	[trace],ax
	lisp$error 'TOO DEEP NESTED'

LISP$SUBRU 'eval',CONEARG,ONEARG
EVAL:
;IFDEF	SCROLL_TEST
	mov	bx,40h
	mov	es,bx
	mov	bx,es:[18h]
	test	bx,10h
	jz	eiind
	push	ax
	priths	<13,10,'Scroll = '>
	call	pprint
	call	ems_reset
	pop	ax
;ENDIF
eiind:
	mov	bx,sp
IFDEF	DXLISP
	cmp	BX,NEW_DX_SP-4000
	jc	toodeep
ELSE
	cmp	bx,1000H
	jc	toodeep
ENDIF
	J$ATOM	AX,EATOM
	CAR	B
	CMP	BX,QUOTE
	JMPN	NZ,EQUOT
	PUSH	BX
	CDR	A
	MOV	BX,[ARGSTA]
	CALL	CONS
	MOV	[ARGSTA],AX
	POP	AX
	push	ax
	MOV	BX,[trace]
	call	atomize
	CALL	CONS
	MOV	[trace],AX
	pop	ax
	frame	evalrest
	CALL	EVAL
	J$nil	ax,evaerr
evacon:	CALL	EVAL
	jmp	framexit
evalrest:
	MOV	BX,[trace]
	CDR	B,BX
	MOV	[trace],BX
DROPARG:MOV	CX,[ARGSTA]
	CDR	C,CX
	MOV	[ARGSTA],CX
	jmp	frame_ret
atomize:r$atom	ax
	car	a
	jmp	atomize

evaerr:	call	crlf
	priths 'function not defined='
	mov	bx,[trace]
	car	a,bx
	call	print
	call	crlf
	priths '		args='
	mov	bx,[argsta]
	car	a,bx
	call	print
	mov	ax,nil
	jmp	evacon

EATOM:	TYPEOF	DL,AX
	WHEN	SYMBOL,ENAME
	when	FSYMBO,EFSYM
	WHEN	SUBRU,ESUBRU	
	RET
EQUOT:	CDR	A
	CAR	A,AX
	RET
ENAME:	CAR	A
	RET
EFSYM:	CAR	B
	MOV	AX,[BX]
	RET


ESUBRU:	CAR	A
	PUSH	AX
	RET

ARG:	MOV	AX,[ARGSTA]
	R$NIL	AX
	CAR	A,AX
	R$NIL	AX
	J$ATOM	AX,OUTOA
NOUTO:	CDR	B
	CAR	A
	RPLACA	[ARGSTA],B
	RET
OUTOA:	CALL	EVAL
	CAR	A,AX
	JMP	NOUTO

EOFARG:	MOV	AX,[ARGSTA]
	CAR	A,AX
	CMP	AX,0
	RET

EVALARG: CALL	ARG
	JMP	EVAL

ONEARG:	MOV	AX,[ARGSTA]
	CAR	A,AX
	CAR	A,AX
	JMP	EVAL
	
TWOARG:	CALL	EVALARG
	PUSH	AX
	CALL	EVALARG
	MOV	BX,AX
	POP	AX
	RET
TREARG:	CALL	EVALARG
	PUSH	AX
	CALL	EVALARG
	PUSH	AX
	CALL	EVALARG
	PUSH	AX
	POP	CX
	POP	BX
	POP	AX
	RET
FOUARG:	CALL	EVALARG
	PUSH	AX
	CALL	EVALARG
	PUSH	AX
	CALL	EVALARG
	PUSH	AX
	CALL	EVALARG
	PUSH	AX
	POP	DX
	POP	CX
	POP	BX
	POP	AX
	RET

NUMERR:	lisp$error 'NOT NUMBER = '

NUMARG:	CALL	EVALARG
NUMVAL:	CMP	AX,FSTNODE
	jC	numvalim
	TYPEOF	DL,AX
	CMP	DL,NUMBER
	jz	numvalcar
	jmp	numerr
numvalcar:
	CAR	A
	RET
numvalim:
	CMP	AX,ZERONUM
	JC	NUMERR
	sub	AX,ZERONUM
	ret
TWONUM:	CALL	NUMARG
	PUSH	AX
	CALL	NUMARG
	MOV	BX,AX
	POP	AX
	RET

lisp$subru 'symbol-string',cantcomp,strarg
	ret

STRARG:	CALL	EVALARG
getstr:	typeof	DL,AX
	CMP	DL,STRING
	RZ
	cmp	dl,fsymbo
	jz	getst2
	CMP	DL,SYMBOL
	JNZ	USTRER
getst2:	CDR	A,AX
	RET
USTRER: LISP$ERROR 'NOT STRING ='

QNAMARg:CALL	ARG
tstnam:	typeof	DL,AX
	CmP	DL,SYMBOL
	rz
	CMP	DL,FSYMBO
	RZ
NAMERR:	LISP$ERROR 'NOT IDENT ='

NAMARG:	CALL	EVALARG
	JMP	tstnam

LISP$SUBRU 'setq',csetq
SETQ:	CALL	QNAMARG
	CMP	DL,FSYMBO
	jz	SETFSY
SET1:	PUSH	AX
	CALL	EVALARG
	POP	BX
	RPLACA	BX,A
	RET
SETFSY:	PUSH	AX
	CALL	EVALARG
	POP	BX
	CAR	B,BX
	MOV	[BX],AX
	RET



csetq:	car$cdr	a,b,bx
	car	b,bx
ccsetq:	call	makefsym
	car	a
	push	ax
	mov	ax,bx
	CALL	NCOMPI
	pop	ax
	jmp	mvtoim


LISP$SUBRU 'set',ctwoarg,twoarg
sett:	typeof	dl,ax
	cmp	dl,fsymbo
	jz	cstfym
	cmp	dl,symbol
	jmpn	z,namerr
	xchg	ax,bx
SETBXAX:rplaca bx,a
	ret
cstFym:	car	a,ax
	xchg	ax,bx
	mov	[bx],ax
	ret



LISP$SUBRU 'defq',cdefq,QNAMARG
DEF1:	PUSH	AX
	CALL	ARG
	POP	BX
	XCHG	AX,BX
	push	ax
	call	SETT
	pop	ax
	RET
cdefq:	car$cdr	a,b,bx
	push	ax
	car	a,bx
	call	csex
	call	mov_bx_ax
	pop	ax
	call	csex
	COCALL	SETT,'SET AX,BX'
	ret

LISP$SUBRU 'abs',cabs,numarg
	test	AH,80H
	jmpn	nz,maknum
	neg	ax
	jmp	maknum
cabs:	call	cnumar1
	compil	0,<TEST AH,80H>,'TEST AH,80H'
	compil	0,<JZ   $+4>,'JZ   $+4'
	jmp	cneg

LISP$SUBRU 'minus',CMINUS,NUMARG
	neg	ax
	jmp	maknum

CMINUS:	CALL	CNUMAR1
CNEG:	compil	0,<NEG AX>,'NEG  AX'
	jmp	cmaknum

LISP$SUBRU 'plus',CPLUS,TWONUM
	add	ax,bx
	jmp	maknum
CPLUS:	CALL	CTWONU1
	COMPIL	0,<ADD AX,BX>,'ADD  AX,BX'
CMAKNUM:mov	[retnumber],0FFH
	RET

LISP$SUBRU 'difference',CDIFF,TWONUM
	sub	ax,bx
	jmp	maknum
CDIFF:	CALL	CTWONU1
	COMPIL	0,<SUB AX,BX>,'SUB  AX,BX'
	JMP	CMAKNUM

LISP$SUBRU 'times',CTIMES,TWONUM
	imul	bx
	jmp	maknum
CTIMES:	CALL	CTWONU1
	COMPIL	0,<IMUL BX>,'IMUL BX'
	JMP	CMAKNUM

LISP$SUBRU 'quotient',CTWONUM,TWONUM
	mov	dx,0
	test	ax,8000h
	jz	qposi
	dec	dx
qposi:	idiv	bx
	jmp	maknum

LISP$SUBRU 'remainder',CTWONUM,TWONUM
	mov	dx,0
	test	ax,8000h
	jz	rposi
	dec	dx
rposi:	idiv	bx
	mov	ax,dx
	jmp	maknum
 
LISP$SUBRU '*&/10000',CTWONUM,TWONUM
	imul	bx
	mov	cx,10000
	idiv	cx
	jmp	maknum

LISP$SUBRU '/&*10000',CTWONUM,TWONUM
	push	bx
	mov	cx,10000
	imul	cx
	pop	bx
	idiv	bx
	jmp	maknum
	
LISP$SUBRU 'add1',CADD1,NUMARG
	INC	AX
	JMP	MAKNUM
CADD1:	CALL	CNUMAR1
	COMPIL	0,<INC AX>,'INC  AX'
	JMP	CMAKNUM

LISP$SUBRU 'sub1',CSUB1,NUMARG
	DEC	AX
	JMP	MAKNUM
CSUB1:	CALL	CNUMAR1
	COMPIL	0,<DEC AX>,'DEC  AX'
	JMP	CMAKNUM


LISP$SUBRU 'zerop',Czerop,ONEARG
	cmp	ax,ZERONUM
	test$resu	z
czerop:	car	a,bx
	call	ncompi_type
	cmp	[retnumber],0
	jz	czero2
	mov	[retnumber],0
	compil	0,<cmp ax,0>,'CMP  AX,0'
	mov	[retzero],t
	ret
czero2:	compil	0,<cmp ax,ZERONUM>,'CMP  AX,ZERO'
	mov	[retzero],t
	ret

LISP$SUBRU 'greaterp',CTWONUM,TWONUM
	add	ax,8000h
	add	bx,8000h
	cmp	bx,ax
	test$resu	c

LISP$SUBRU 'lessp',CTWONUM,TWONUM
	add	ax,8000h
	add	bx,8000h
	cmp	ax,bx
	test$resu	c

LISP$SUBRU 'eqn',CEQN,TWONUM
	cmp	ax,bx
	test$resu	z
CEQN:	CALL	CTWONU1
	COMPIL	0,<CMP AX,BX>,'CMP  AX,BX'
	mov	[retzero],t
	RET

LISP$SUBRU 'last',CONEARG,ONEARG
LAST:	R$ATOM	AX
	cdr	b
	R$ATOM	BX
	mov	ax,bx
	Jmp	LAST

LISP$SUBRU 'list',clist
argLIST:MOV	AX,[ARGSTA]
	car	a,ax
ALIST:	J$ATOM	AX,EVAL
	car	a
	cdr	b
	PUSH	BX
	CALL	EVAL
	mov	bp,sp
	xchg	AX,[bp]
	CALL	ALIST
	POP	BX	
	xchg	BX,AX
	jmp	CONS
clist:	j$atom	bx,clist3
	car$cdr	a,b,bx
	j$nil	bx,clist2
	push	bx
	call	stackarg
	pop	bx
	call	clist
	call	mov_bx_ax
	call	POP_AX
	cocall	cons,'CONS'
	ret
clist3:	mov	ax,bx
	jmp	ncompi

clist2:	call	ncompi
	compil	0,<MOV BX,NIL>,'MOV  BX,NIL'
	cocall	cons,'CONS'
	ret

LISP$SUBRU 'reverse',cONEARG,ONEARG
REVERSE:MOV	CX,NIL
REVERS:	J$ATOM	AX,REVEZ
	CAR	A
	CDR	b
	PUSH	BX
	MOV	BX,CX
	CALL	CONS
	MOV	cx,ax
	POP	AX
	Jmp	REVERS
REVEZ:	mov	ax,cx
	RET


LISP$SUBRU 'nreverse',CONEARG,ONEARG
	R$ATOM	AX
	MOV	CX,NIL
NREVER: PUSH	AX
	CDR	B,AX
	rplacd	AX,C
	xchg	BX,AX
	J$ATOM	AX,NREVZ
	POP	CX
	Jmp	NREVER
NREVZ:	POP	AX
	RET


LISP$SUBRU 'rplaca',cTWOARG,TWOARG
	r$atom	ax
	RPLACA	AX,B
	RET

LISP$SUBRU 'rplacd',cTWOARG,TWOARG
	R$ATOM	AX
	rplacd	ax,b
	RET

LISP$SUBRU 'and',cand
	MOV	AX,[ARGSTA]
	CAR	B,AX	
ANDI:	R$NIL	bx
	CAR	A,BX
	CDR	B
	PUSH	BX
	CALL	EVAL
	POP	BX
	R$NIL	ax
	JMP	ANDI
CAND:	PUSH	BX
	MOV	AX,[PCLOC]
	ADD	AX,3
	PUSH	AX
	ADD	AX,3
	push	ax
	call	cjmp
	pop	ax
	MOV	[pcloc],ax
CANDL:	mov	bp,sp
	MOV	bx,[BP+2]
	car$cdr	a,b,bx
	J$NIL	BX,CANDE
	MOV	[BP+2],BX
	call	ncompi_type
	mov	al,[retzero]
	mov	[retzero],0
	cmp	al,0
	jz	cand3
	compil	0,<mov ax,0>,'MOV  AX,NIL'
	call	skpnil
	jmp	cand4
cand3:	call	testnil
	call	skpt
cand4:	pop	ax
	push	ax
	CALL	CJMP
	JMP	CANDL
CANDE:	CALL	NCOMPI
	POP	CX
	CALL	JMPFRCXHERE
	POP	BX
	RET

LISP$SUBRU 'or',cor
ORR:	CALL	EOFARG
	JmpN	NZ,NILRET
	CALL	EVALARG
	J$NIL	ax,ORR
	RET
COR:	PUSH	BX
	MOV	AX,[PCLOC]
	ADD	AX,3
	PUSH	AX
	ADD	AX,3
	push	ax
	call	cjmp
	pop	ax
	MOV	[pcloc],ax
CORL:	mov	bp,sp
	MOV	bx,[BP+2]
	car$cdr	a,b,bx
	J$NIL	BX,CORE
	MOV	[BP+2],BX
	call	ncompi
	call	testnil
	call	skpnil
	pop	ax
	push	ax
	CALL	CJMP
	JMP	CORL
CORE:	CALL	NCOMPI
	POP	CX
	CALL	JMPFRCXHERE
	POP	BX
	RET

LISP$SUBRU 'nconc',CTWOARG,TWOARG
NCONC:	J$ATOM	AX,NCONC2
	PUSH	AX
	PUSH	BX
	CALL	LAST
	POP	BX
	RPLACD	AX,B
	POP	AX
	RET
NCONC2:	MOV	AX,BX
	RET

LISP$SUBRU 'memeq',CTWOARG,TWOARG
MEMEQ:	J$NIL	bx,NILRET	
	PUSH	BX
	PUSH	AX
	CAR	B,BX
	CMP	AX,BX
	POP	BX
	POP	AX
	RZ
	CDR	A,AX
	xchg	BX,AX
	JMP	MEMBER

LISP$SUBRU 'member',CTWOARG,TWOARG
MEMBER:	J$NIL	bx,NILRET	
	PUSH	BX
	PUSH	AX
	CAR	B,BX
	CALL	EQUAL
	POP	BX
	POP	AX
	RZ
	CDR	A,AX
	xchg	BX,AX
	JMP	MEMBER


LISP$SUBRU 'assoc',CTWOARG,TWOARG
ASSOC:	J$ATOM	BX,NILRET	
	PUSH	BX
	PUSH	AX
	CAR	B,BX
	J$ATOM	BX,ASSOC2
	CAR	B,BX
	CALL	EQUAL
	JZ	HLCAR
ASSOC2:	POP	AX
	POP	BX
	CDR	B,BX
	JMP	ASSOC
HLCAR:	POP	AX
	POP	AX
	CAR	A,AX
	RET

LISP$SUBRU 'not',CNOT,ONEARG
	cmp	ax,0
	test$resu	z
LISP$SUBRU 'null',CNOT,ONEARG
	cmp	ax,0
	test$resu	z
CNOT:	car	a,bx
	call	ncompi
	COMPIL	0,<CMP AX,0>,'CMP  AX,NIL'
	mov	[retzero],t
	RET


LISP$SUBRU 'depthl',cantcomp,NUMARG
	PUSH	AX
	CALL	EVALARG
	POP	BX
DEPTHL:
	J$NIL	bx,NILRET
	J$ATOM	AX,TRET
	DEC	BX
	PUSH	AX
	CAR	A,AX
	CALL	dePTHL
	POP	AX
	CDR	A,AX
	JMP	DEPTHL

LISP$SUBRU 'length',CONEARG,ONEARG
lengt:	MOV	BX,0
LELO:	J$ATOM	AX,LDON
	CDR 	A
	INC	BX
	JMP	LELO
LDON:	xchg	BX,AX
	jmp	MAKNUM

LISP$SUBRU 'numberp',CONEARG,ONEARG
NMBR:	typeOF	DL,AX
	WHEN	NUMBER,TRET
	jmp	NILRET

LISP$SUBRU 'stringp',CONEARG,ONEARG
	TYPEOF	DL,AX
	When	string,Tret
	jmp	NILRET

LISP$SUBRU 'identp',CONEARG,ONEARG
	TYPEOF	DL,AX
	When	symbol,Tret
	when	fsymbo,tret
	jmp	NILRET

LISP$SUBRU 'atom',CONEARG,ONEARG
ATOM:	J$ATOM	AX,TRET
	jmp	NILRET

dataseg	segment
	cyclic	dw nil
dataseg	ends

LISP$SUBRU 'cyclicp',CONEARG,ONEARG
cyclicp:mov	word ptr [cyclic],nil
	call	cycli1
	mov	ax,[cyclic]
	ret

cycli1:	cmp	word ptr [cyclic],nil
	rnz
	J$ATOM	AX,CYCLI2
	or	dl,circul
	settype	ax,dl
	push	ax
	car	a
	call	CYCLI1
	pop	AX
	push	ax
	cdr	a,ax
	call	cycli1
	pop	ax
	typeof	dl,ax
	and	dl,not circul
	settype	ax,dl
	RET
CYCLI2:	typeof	dl,AX
	test	dl,circul
	rz
	mov	word ptr [cyclic],t
	ret

logop macro op
	op	AX,BX
	jmp	maknum
	endm

LISP$SUBRU 'logand',Cband,TWONUM
	LOGOP	AND
cband:	call	ctwonu1
	compil	0,<AND AX,BX>,'AND  AX,BX'
	JMP	CMAKNUM

LISP$SUBRU 'logor',Cbor,TWONUM
	LOGOP	OR
cbor:	call	ctwonu1
	compil	0,<OR AX,BX>,'OR   AX,BX'
	JMP	CMAKNUM

LISP$SUBRU 'logxor',Cbxor,TWONUM
	LOGOP	XOR
cbxor:	call	ctwonu1
	compil	0,<XOR AX,BX>,'XOR  AX,BX'
	JMP	CMAKNUM


purge logop

LISP$SUBRU 'tab',cONEARG,EVALARG
	J$NIL	ax,ATAB
	CALL	NUMVAL
	MOV	BX,AX
DOTAB:
BTAB:	cmp	byte ptr[tabs],bl
	jmpN	C,NILRET
	MOV	dl,20H
	CALL	PRINTC
	JMP	BTAB
ATAB:	MOV	dl,[TABS]
ascii:	mov	ah,0
	mov	al,dl
	ADD	AX,ZERONUM
	ret

LISP$SUBRU 'pprint',CONEARG,ONEARG
	PUSH	AX
	CALL	PPRINT
	POP	AX
	RET

PPRInt:	MOV	bl,[TABS]
PPRIN2:
	PUSH	BX
	PUSH	AX
	MOV	BX,15
	CALL	dePTHL
	cmp	ax,0
	POP	AX
	POP	BX
	JmPN	Z,PRINT
	INC	bl
	MOV	dl,'('
	CALL	PRINTC
PPR3:	PUSH	BX	
	PUSH	AX
	CAR	A,AX
	J$ATOM	AX,PPRATO
PPRANO:	CALL	PPRIN2
	POP	AX
	CDR	A,AX
	J$ATOM	AX,PPRE
PPRAEJ:	POP	BX
	PUSH	BX
	PUSH	AX
	CALL	CRLF
	CALL	DOTAB
	POP	AX
PPRATE:	POP	BX
	jmp	PPR3
PPRATO:	MOV	bl,[TABS]
	Cmp	bl,50
	JMPn	C,PPRANO
	CALL	PRINT	
	POP	AX
	CDR	A,AX
	J$ATOM	AX,PPRE
	CAR	B,AX
	J$ATOM	BX,PRRATJ
	JMP	PPRAEJ
PRRATJ:	MOV	dl,20H
	CALL	PRINTC
	JMP	PPRATE
PPRE:	POP	BX
	J$NIL	ax,PPRE2
	PUSH	AX
	PRITHS ' . '
	POP	AX
	CALL	PRINT
PPRE2:	MOV	dl,')'
	jmp	PRINTC


EQUAL:	cmp	ax,bx
	rz
	J$ATOM	AX,EQ$ATO
	J$ATOM	bX,NOEQ
	PUSH	AX
	PUSH	BX
	CAR	A,AX
	XCHG	BX,AX
	CAR	A,AX
	CALL	EQUAL
	POP	BX
	POP	AX
	RNZ
	CDR	A,AX
	XCHG	BX,AX
	CDR	A,AX
	JMP	EQUAL
EQ$ATO:	XCHG	BX,AX
	J$ATOM	AX,EQ$AT2
NOEQ:	mov	al,1
	cmp	al,0
	RET
EQ$AT2:
	TYPEOF	DL,AX
	TYPEOF	DH,BX
	CMP	DL,DH
	RNZ
	WHEN	STRING,STREQU
	CMP	DL,NUMBER
	RNZ
	CAR	C,AX
	CAR	D,BX
	CMP	CX,DX
	RET

STREQU:	cmp	ax,bx
	RZ
	J$NIL	AX,NOEQ
	J$NIL	BX,NOEQ
	PUSH	AX
	PUSH	BX
	CAR	A,AX
	CAR	B,BX
	cmp	ax,bx
	POP	BX
	POP	AX
	JMPN	Z,NOEQ
	CDR	B,BX
	CDR	A,AX
	JMP	STREQU		


LISP$SUBRU 'equal',CTWOARG,TWOARG
	CALL	EQUAL
	test$resu	z


LISP$SUBRU 'eq',CEQ,TWOARG
	cmp	ax,bx
	test$resu	z
CEQ:	call	ctwoar1
	compil	0,<cmp ax,bx>,'CMP  AX,BX'
	mov	[retzero],t
	ret

;===== lambdat ja prognit

pushbx	macro pino
	mov	ax,[pino]
	xchg	ax,bx
	call	cons
	mov	[pino],ax
	endm

popbx	macro	pino
	mov	ax,[pino]
	car$cdr	b,a,ax
	mov	[pino],ax
	endm

LISP$SUBRU 'setset',CTWOARG,TWOARG	
SETSET:	R$NIL	AX
	J$ATOM	AX,SETSE2
	CDR	c
	PUSH	CX
	car	a
	J$ATOM	BX,SETNIL
	CDR	c
	PUSH	CX
	car	b
	jmp	NOSETN
SETNIL:	MOV	BX,NIL
	PUSH	BX	
NOSETN:	CALL	SETSET
	POP	BX
	POP	AX
	jmp	SETSET
SETSE2:	CALL	SETT
	RET

	;AX= ids
GETGET: R$NIL	AX
	J$ATOM	AX,GETGE2
	car$cdr	a,b,ax
	push	bx
	CALL	GETGET
	mov	bp,sp
	xchg	AX,[bp]
	CALL	GETGET
	POP	BX
	XCHG	BX,AX
	jmp	CONS
GETGE2:	TYPEOF	DL,AX
	CMP	DL,SYMBOL
	jmpn	nz,eatom
	cmp	dl,fsymbo
	jmpn	nz,eatom
	jmp	LAMBERR



LAMBERR:
	LISP$ERROR 'NOT A NAME ='

	
SAVEVAR:PUSH	AX
	CALL	GETGET
	POP	BX
	PUSH	BX
	XCHG	BX,AX
	CALL	CONS
	XCHG	BX,AX
	pushbx	ENVIRO
	POP	AX
	RET

RSTOVAR:PUSH	AX
	popbx	ENVIRO	
	CAR$CDR	A,B,BX
	CALL	SETSET
	POP	AX
FRAME_RET:
	RET
	POP	CX
	JMP	FRAMEXIT

	;AX=[(id) (id) (id))
LETVAR: R$NIL	AX
	car	b,ax
	cdr	a
	CAR	B,BX
	typeof	dl,bx
	when	SYMBOL,letva2
	when	FSYMBO,letva2
	lisp$error 'BAD LET = '


letva2:	PUSH	BX
	CALL	LETVAR
	XCHG	BX,AX
	POP	AX
	jmp	CONS

lisp$subru 'let-functions',Cletfun,ARG
	push	ax
	call	letquot
	mov	bp,sp
	xchg	AX,[bp]
	CALL	LETVAR
	jmp	let_f_jamat

cletFUN:
	CAR$CDR	A,B,BX
	PUSH	BX
	PUSH	AX
	CALL	LETVAR
	push	ax
	call	pusvars
	pop	ax
	push	ax
	CALL	SAVEVAR
	pop	bx
	POP	AX
	push	bx
	call	letcomp
	pop	ax
	pop	bx
	push	ax
	frame	cletfu2
	CALL	compblock
	jmp	framexit
cletfu2:
	call	RSTOVAR
	pop	ax
	jmp	popvars

LETCOMP:
	R$NIL	AX
	CAR$CDR	A,B,AX
	PUSH	BX
	car$cdr	a,b,ax
	push	ax
	CAR	b,bX
	push	bx
	call	sett
	pop	ax
	CALL	cfunC1
	mov	bx,ax
	pop	ax
	push	ax
	push	bx
	call	sett
	pop	bx
	pop	ax
	call	ccsetq
	pop	ax
	jmp	LETCOMP


LETQUOT:
	R$NIL	AX
	CAR$CDR	A,B,AX
	PUSH	BX
	CDR	A,AX
	CAR	A,AX
	mov	bp,sp
	xchg	AX,[bp]
	CALL	LETQUOT
	POP	BX
	XCHG	BX,AX
	jmp	CONS

LISP$SUBRU 'progn',CompPROGN
PROGN:	MOV	AX,[ARGSTA]
	CAR	B,AX
	xor	ax,ax
PROGN2:
	R$NIL	BX
	typeof	dl,bX
	Cmp	dl,LIST
	jmpn	Z,eval	
	car	a
	cdr	b
	PUSH	BX
	CALL	EVAL
	POP	BX
	jmp	PROGN2
LPROGN:	XCHG	BX,AX
	xor	ax,ax
	jmp	PROGN2

cframe:
	call	cconst
	call	push_AX
cframe1:
	compil	0,<PUSH [STACKMARK]>,'PUSH [STACKMARK]'
	compil	0,<MOV[STACKMARK],SP>,'MOV  [STACKMARK],SP'
	ret

COMPBLOCK:
	call	spaceforjump
	push	CX
	PUSH	BX
	call	cframe1
	POP	BX
	CALL	compprogn
	mov	ax,offset framexit
	call	cjmp
callabove:
	pop	cx
	push	cx
	call	jmpfrcxhere
	pop	ax
	ADD	AX,3
	CALL	CCALL
	RET

COMPPROGN:
	xor	ax,ax
	j$nil	bx,ncompi
COMPPROG2:
	R$NIL	BX
	j$atom	bx,ncompi
	car	a,bx
	cdr	b
	PUSH	BX
	CALL	NCOMPI
	POP	BX
	jmp	COMPPROG2

lisp$subru 'prog1',cprog1
	call	evalarg
	push	ax
	call	progn
	pop	ax
	ret

cprog1:	car$cdr	a,b,bx
	push	bx
	call	stackarg
	pop	bx
	call	compprogn
	call	POP_AX
	RET

LISP$SUBRU 'fsymbols',cnoarg
	mov	ax,oblist
fsyms1:	R$NIL	AX
	car$cdr	b,a,ax
	typeof	dl,bx
	when	fsymbo,fsyms2
	jmp	fsyms1
fsyms2:	push	bx
	call	fsyms1
	pop	bx
	xchg	ax,bx
	jmp	cons

LISP$SUBRU 'progv',cantcomp,EVALARG
	jmp	PROG

LISP$SUBRU 'prog',CPROG,ARG	
PROG:	CALL	SAVEVAR
	FRAME	RSTOVAR
	CALL	PROGN ; rapaaa
	jmp	framexit

Cprog:	PUSH	BX
	COcall	SAVEVAR,'SAVEVAR'
	POP	BX
	CALL	COMPBLOCK
	COcall	RSTOVAR,'RSTOVAR'
	RET

LISP$SUBRU  'mlambda',cantcomp
	CALL	NLAMBA
	jmp	MLAMBA
LISP$SUBRU 'macro',cantcomp
	CALL	NSL
MLAMBA:	MOV	BX,[ARGSTA]
	PUSH	BX
	frame	rstoargs
	CDR	B,BX
	CDR	B,BX
	MOV	[ARGSTA],BX
	CALL	EVAL
	jmp	framexit

rstoargs:POP	[ARGSTA]
	jmp	frame_ret

LISP$SUBRU 'nslambda',cantcomp
  NSL:	MOV	AX,[ARGSTA]
	CDR	A,AX
	jmp	AMBDA

LISP$SUBRU 'nlambda',cantcomp
NLAMBA:	MOV	AX,[ARGSTA]
	CDR	A,AX
	CAR	A,AX
AMBDA:	PUSH	AX	; new
	CALL	ARG
let_f_jamat:
	CALL	SAVEVAR
	POP	BX
	CALL	SETSET	; set new
	frame	rstovar
	call	progn
	jmp	framexit

framexit:
	MOV	SP,[STACKMARK]
	POP  	[STACKMARK]
	RET


lisp$subru 'catch',ccatch
	call	evalarg
	push	ax
	frame	catchexit
	call	evalarg
	jmp	framexit
ccatch:	call	spaceforjump
	push	cx
	car$cdr	a,b,bx
	push	bx
	call	ncompi
	call	push_AX
	mov	ax,offset catchexit
	call	cframe
	POP	AX
	call	scar
	call	ncompi
	mov	ax,offset framexit
	call	cjmp
	jmp	callabove
	
catchexit:
	POP	cx
	cmp	cx,[throwtag]
	jz	catch_end
	ret
catch_end:
	MOV	BYTE PTR CS:[FRAME_RET],0C3H ; RET
	ret
	
lisp$subru 'throw',ctwoarg,twoarg
	mov	[throwtag],ax
	mov	ax,bx
	MOV	BYTE PTR CS:[FRAME_RET],90H ; NOP
	mov	bx,offset throw$err
	mov	[erheen_syy],bx
	jmp	framexit

dataseg segment
     throw$err db 'NO CATCH FOR THROW ',0
dataseg ends

idlist:	cmp	ax,nil
	rz
	typeof	dl,ax
	cmp	dl,list
	rnz
	car$cdr	a,b,ax
	typeof	dl,ax
	cmp	dl,fsymbo
	jz	idlis2
	cmp	dl,symbol
	rnz
	call	makefsym
idlis2:	mov	ax,bx
	jmp	idlist

LISP$SUBRU 'lambda',clambda
	MOV	AX,[ARGSTA]
	CDR	A,AX
	CAR	A,AX
	CALL	ALIST
	jmp	AMBDA
clambda:	
	car$cdr	a,b,bx
	j$nil	ax,compblock
	push	bx
	push	ax
	call	savevar
	mov	bx,nil
	call	setset
	pop	ax
	pop	bx
	frame	RSTOVAR
	call	clambda2
	jmp	framexit

clambda2:
	push	bx
	push	ax
	call	idlist
	jmpn	z,lamarn
	pop	ax
	push	ax
	CDR	A,AX
	J$nil	AX,LAMAR1
	CDR	A,AX
	J$NIL	AX,LAMAR2
	CDR	A,AX
	J$NIL	AX,LAMAR3
	CDR	a,AX
	J$NIL	AX,LAMAR4
	JMP	LAMARN

PPUSV:	CAR$CDR	A,B,BX
	car	a,ax
	ret

lamrest:
	POP	AX
	POP	BX
	PUSH	AX
	CALL	COMPBLOCK
	POP	AX
	JMP	POPVARS
	
LAMAR1:	cocall	onearg,'ONEARG'
	mov	AX,offset conearg
	mov	[functype],AX
	POP	ax
	push	ax
	call	pusvars
	pop	bx
	push	bx
	call	ppusv
	call	mvtoim
	jmp	lamrest


LAMAR2:	cocall	TWOarg,'TWOARG'
	mov	AX,offset cTWOarg
	mov	[functype],AX
	POP	ax
	push	ax
	call	pusvars
	pop	bx
	push	bx
	call	ppusv
	call	mvtoim
	call	ppusv
	call	mvtbim
	jmp	lamrest

LAMAR3:	cocall	TREarg,'TREARG'
	mov	AX,offset cTREarg
	mov	[functype],AX
	POP	ax
	push	ax
	call	pusvars
	pop	bx
	push	bx
	call	ppusv
	call	mvtoim
	call	ppusv
	call	mvtbim
	call	ppusv
	call	mvtcim
	jmp	lamrest

LAMAR4:	cocall	fouarg,'fouARG'
	mov	AX,offset cfouarg
	mov	[functype],AX
	POP	ax
	push	ax
	call	pusvars
	pop	bx
	push	bx
	call	ppusv
	call	mvtoim
	call	ppusv
	call	mvtbim
	call	ppusv
	call	mvtcim
	call	ppusv
	call	mvtdim
	jmp	lamrest

LAMARN:	cocall	ARGLIST,'ARGLIST'
	call	push_AX
	mov	AX,offset carglist
	mov	[functype],AX
	pop	ax
	call	csex
	coCALL	SAVEVAR,'SAVEVAR'
	compil	0,<POP	BX>,'POP  BX'
	coCALL	SETSET,'SETSET'
	pop	bx
	CALL	COMPBLOCK
	COCALL	RSTOVAR,'RSTOVAR'
	RET

pusvar:	push	ax
	call	makefsym
	compil	2,<push [trace]>,'PUSH ['
	car	a,ax
	call	cword
	comptext ']'
	pop	ax
	ret
popvar:	push	ax
	compil	2,<POP [trace]>,'POP  ['
	car	a,ax
	call	cword
	comptext ']'
	pop	ax
	ret

pusvars:R$NIL	AX
	CAR$CDR	A,B,aX
	PUSH	BX
	call	pusvar
	POP	AX
	JMP	pusvars

popvars:CALL	REVERSE
POPVA2:	R$NIL	AX
	CAR$CDR	A,B,aX
	PUSH	BX
	call	popvar
	POP	AX
	JMP	POPVA2


	
LISP$SUBRU 'cond',ccond
cond:	CALL	ARG
	R$NIL	AX
	CAR$CDR	B,A,AX
	XCHG	BX,AX
	PUSH	BX
	CALL	EVAL
	POP	BX
	J$NIL	AX,COND
	jmp	PROGN2
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
LISP$SUBRU 'repeat-times',crepeattimes,NUMARG
	PUSH	AX
	MOV	AX,[ARGSTA]
	CAR	A,AX
	POP	BX
REPTI:	J$NIL	bx,NILRET
	PUSH	BX
	PUSH	AX
	CALL	LPROGN
	POP	AX
	POP	BX
	DEC	BX
	Jmp	REPTI

LISP$SUBRU 'if',cif,EVALARG
	cmp	ax,0
	JNZ	THENN
	CALL	ARG
THENN:	JmP	EVALARG	

LISP$SUBRU 'repeat-until',CREPEUN
	MOV	AX,[argsta]
	car	a,AX
RLOOP:	PUSH	AX
	CAR$cdr	b,a,AX
	PUSH	AX
	xchg	bx,AX
	CALL	EVAL
	JN$NIL	ax,EREP
	POP	AX
	CALL	LPROGN
	POP	AX
	JMP	RLOOP
EREP:	POP	bx
	POP	bx
	RET

LISP$SUBRU 'while',CWHILE
	MOV	AX,[argsta]
	car	a,AX
WLOOP:	PUSH	AX
	CAR$cdr	b,a,AX
	PUSH	AX
	xchg	bx,AX
	CALL	EVAL
	J$NIL	ax,EWHI
	POP	AX
	CALL	LPROGN
	POP	AX
	JMP	WLOOP
EWHI:	POP	bx
	POP	bx
	RET

LISP$SUBRU 'repeat',crepeat
	MOV	AX,[argsta]
	car	a,AX
REPEA:	PUSH	AX
	CALL	LPROGN
	POP	bx
	CMP	AX,0
	RNZ
	xchg	bx,AX
	JMP	REPEA

LISP$SUBRU 'explode',CONEARG,ONEARG
EXPLODE:typeof	dl,ax
	WHEN	LIST,NILRET
	WHEN	symbol,EXNAM
	when	fsymbo,exnam
	WHEN	NUMBER,EXNUM
	WHEN	STRING,EXSTRI
	RET
EXNAM:	CDR	a,ax
EXSTRI:	CALL	EXSTR2
	JMP	EXP13	
EXNUM:	car	a
	CALL	EXPLNU
EXP13:	MOV	bx,OFFSET ITEMB
EXP3:	MOV	DL,[bx]
	CMP	DL,0
	JMPN	NZ,NILRET
	PUSH	bx
	CALL	ASCII
	POP	BX
	PUSH	AX
	INC	bx
	CALL	EXP3
	POP	bx
	XCHG	AX,BX
	JMP	CONS

AYCMRS:	MOV	BX,OFFSET ITEMB
COMP2:	R$NIL	AX
	CAR$CDR	C,a,ax
	TYPEOF	DL,CX
	CMP	DL,NUMBER
	JMPN	Z,NUMERR
	CAR	D
	MOV	[BX],DL
	INC	BX
	Jmp	COMP2

LISP$SUBRU 'str-compress',CONEARG,ONEARG
STRCRS:	CALL	AYCMRS
	MOV	DL,0
	MOV	[BX],DL
	MOV	BX,OFFSET ITEMB
	JMP	MAKSTR

LISP$SUBRU 'compress',CONEARG,ONEARG
CMPRSS:	CALL	AYCMRS
RETUEN:	JMP	ITEMC

EXSTR2:	MOV	BX,OFFSET ITEMB
exstr3:	mov	cx,0
GCRS2:	J$NIL	AX,GCRZ
	typeof	dl,ax
	cmp	dl,STRING
	jmpn	z,voi_paska
	CAR	D
	CDR	A
	MOV	[BX],DX
	add	cx,2
	add	bx,2
	JMP	GCRS2
GCRZ:	MOV	byte ptr [BX],0
	RET	
voi_paska:
	mov	bx,offset itemb
	call	prints
	jmp	ustrer

LISP$SUBRU 'car',CONEARG,ONEARG
SCAR:	J$ATOM	AX,NILRET
	CAR	A,AX
	RET
LISP$SUBRU 'cdr',CONEARG,ONEARG
SCDR:	J$ATOM	AX,NILRET
	CDR	A,AX
	RET
LISP$SUBRU 'cadr',CONEARG,ONEARG
CADR:	CALL	SCDR
	jmp	SCAR
LISP$SUBRU 'cddr',CONEARG,ONEARG
CDDR:	CALL	SCDR
	jmp	SCDR
LISP$SUBRU 'cdddr',CONEARG,ONEARG
CDDDR:	CALL	CDDR
	jmp	SCDR
LISP$SUBRU 'caddr',CONEARG,ONEARG
CADDR:	CALL	CDDR
	jmp	SCAR
LISP$SUBRU 'cadddr',CONEARG,ONEARG
CADDDR:	CALL	CDDDR
	jmp	SCAR
LISP$SUBRU 'caar',CONEARG,ONEARG
CAAR:	CALL	SCAR
	jmp	SCAR
LISP$SUBRU 'cdxr',ctwoarg,twoarg
	call	NUMVAL
	XCHG	AX,BX
CDXRL:	R$NIL	BX
	CALL	SCDR
	DEC	BX
	Jmp	CDXRL

LISP$SUBRU 'readc',CNOARG
	CALL	READC
	jmp	ascii

LISP$SUBRU 'readc-bin',cnoARG
	MOV	bx,[INCHN]
	mov	dx,offset onech
	mov	cx,1
       	msdos	3fh
	jmpn	nc,ioerr
	cmp	ax,0
	jmpn	nz,nilret
	mov	dl,[onech]
	JMP	ASCII

LISP$SUBRU 'nxtch',CNOARG
	mov	dl,[nxtch]
	JmP	ASCII

IFNDEF DXLISP
lisp$subru 'readcc',CNOARG
	MOV	AL,80H ; nokia slow
	OUT	22H,AL
	MOV	AL,18H
	OUT	23H,AL
	mov	AH,7
	INT	21h
	mov	dl,al
	MOV	AL,80H ; nokia fast
	OUT	22H,AL
	MOV	AL,4
	OUT	23H,AL
	JMP	ASCII
ENDIF

LISP$SUBRU 'printc',CNUMARG,NUMARG
	mov	dl,al
	CALL	PRINTC
	JmP	NILRET

LISP$SUBRU 'cname',cantcomp,QNAMARG
	PUSH	AX
	CALL	QNAMARG
	POP	BX
	cdr	c,ax
	cdr	d,bx
	rplacd	ax,d
	rplacd	bx,c
	JmP	TRET


LISP$SUBRU 'getchar',ctwoarg,twoarg
	cdr	a,ax
	PUSH	AX
	mov	ax,bx
	call	numval
	mov	bx,ax
	POP	AX
	DEC	bl
MOOCDR:	test	bl,0FEH
	JZ	GETC$3
	CDR	A,AX
	DEC	bl
	DEC	bl
	Jmp	MOOCDR
GETC$3:	CAR	A,AX
	cmp	bl,0
	JNZ	TOKACH
	MOV	dl,aL
	JmP	ASCII
TOKACH:	MOV	dl,aH
	JmP	ASCII

;===========================================================


LISP$SUBRU 'enviro',CNOARG
	MOV	AX,[ENVIRO]
	RET

LISP$SUBRU 'argsta',CNOARG
	MOV	AX,[ARGSTA]
	RET

LISP$SUBRU 'trace',cantcomp,EVALARG
	MOV	BX,[TRACE]
	MOV	[TRON],AX
	XCHG	BX,AX
	RET


LISP$SUBRU 'addr-of',cantcomp,EVALARG
	Jmp	MAKNUM

LISP$SUBRU 'at-addr',CNUMARG,NUMARG
	RET


LISP$SUBRU 'oblist',CNOARG
	MOV	AX,[OBLIST]
	RET

LISP$SUBRU 'garbage',conearg,onearg
garbage:cmp	ax,fstnode
	jmpn	nc,nilret
	cmp	ax,[lastnode]
	jmpn	c,nilret
	mov	bx,[freelist]
	rplacd	ax,b
	mov	[freelist],ax
	mov	ax,t
	ret
	

;===============================================
; io ==================================

dataseg	segment
	itemlen	db	0
		db	0
	ITEMB		DB 200H DUP(?)
	ITEMB2		DB 200H DUP(?)
	ITEMBP		DW ITEMB	
	echo		db	0
	TABS		DB 0
	NXTCH		DB 0
	INCHN		Dw	0
	OUTCHN 		Dw	0
	filename	dw	0
	onech	db	0

dataseg	ends

lisp$subru 'echo',cantcomp,evalarg
	mov	[echo],al
	ret

lisp$subru 'out',CONEARG,EVALARG
	j$NIL	ax,goutchn
	call	NUMVAL
	mov	[outchn],ax
	mov	byte ptr [tabs],0
	jmp	tret
goutchn:mov	ax,[outchn]
	jmp	maknum

lisp$subru 'in',CONEARG,EVALARG
	j$NIL	ax,ginchn
	call	NUMVAL
	mov	[inchn],ax
	mov	dl,0
	mov	[nxtch],dl
	jmp	tret
ginchn:	mov	ax,[inchn]
	jmp	maknum

IFNDEF	DXLISP
READC:	PUSH	AX
	PUSH	BX
	PUSH	CX
	mov	dl,[nxtch]
	push	dx
	cmp	dl,1ah
	jz	eeee
	call	redimm
	mov	[nxtch],dl
eeee:	POP	DX
	POP	CX
	POP	BX
	POP	AX
	RET

REDIMM:	MOV	bx,[INCHN]
	mov	dx,offset onech
	mov	cx,1
       	msdos	3fh
	jc	ioerr
	cmp	ax,0
	jz	eofile
	mov	dl,[onech]
	cmp	[echo],0
	rz
	msdos	6
	mov	dl,[onech]
	ret
eofile: mov	dl,1ah
	ret
ioerr:	call	maknum
	lisp$error ' IO-ERROR = '

PRINTC:	PUSH	AX
	PUSH	BX
	PUSH	CX
	PUSH	DX
	mov	bx,offset tabs
	inc	byte ptr [bx]
	cmp	dl,13
	jnz	rivisi
	mov	byte ptr [bx],-1
rivisi:	mov	[onech],dl
	cmp	[echo],0
	jz	eiecho
	msdos	6
eiecho:	MOV	bx,[outchn]
	cmp	bx,0
	jnz	prntc2
	mov	al,[g_char_mode]
	cmp	al,0
	jz	prntc2
	mov	al,dl
	call	graphc
	jmp	short noprint
prntc2:	mov	dx,offset onech
	mov	cx,1
	msdos	40h
	jc	ioerr
noprint:POP	DX
	POP	CX
	POP	BX
	POP	AX
	RET

lisp$subru 'rename-file',cantcomp,strarg
	push	ax
	call	strarg
	mov	bx,offset itemb2
	call	exstr3
	pop	ax
	call	exstr2
	mov	dx,offset itemb
	MOV	AX,DS
	mov	es,AX
	mov	di,offset itemb2
	msdos	56H
	test$resu	nc


lisp$subru 'find-first',cantcomp
	mov	dx, offset itemb2
	msdos	1ah
	call	strarg
	call	exstr2
	call	numarg
	mov	dx,offset itemb
	mov	cx,ax
	msdos	4eh
ffi2:	jmpn	nc,nilret
	mov	bx,offset itemb2[30]
	jmp	retuid
lisp$subru 'find-next',cantcomp,numarg
	mov	dx,offset itemb2
	mov	cx,ax
	msdos	4fh
	jmp	ffi2


LISP$SUBRU 'open',cantcomp,strarg
open:	mov	[filename],ax
	call	exstr2
opeite:	mov	dx,offset itemb
	mov	al,2 ; read/write
	msdos	3dh
	jc	creerr
	jmp	maknum


LISP$SUBRU 'create',cantcomp,strarg
CREATE:	mov	[filename],ax
	call	exstr2
	mov	dx,offset itemb
	mov	cx,0
	msdos	3ch
	jc	creerr
	jmp	maknum

creerr:	call	maknum
	mov	bx,[filename]
	call	cons
	lisp$error 'file error ='

Lisp$subru 'close',CNUMARG,NUMARG

CLOSE:	mov	bx,ax
	msdos	3eh
	jc	creerr
	jmp	tret
	

lisp$subru 'nquit',CNUMARG,NUMARG
	push	AX
	cmp	[EMS_ON],0
	jz	nquit2
	MOV	DX,emm_handle		 ; load EMM handle
	MOV	AH,45h			 ; load function code
	INT	67h			 ; call the memory manager
	OR	AH,AH			 ; check EMM status
	JMPN	Z,emm_err_handler	 ; jump to error handler on error
nquit2:
	POP	AX
	msdos	4ch

ENDIF

dataseg segment
	asciizb db 200h dup (0)
dataseg ends

lisp$subru 'ASCIIZ',cantcomp,numarg
	push	ax
	call	strarg
	pop	bx
	add	bx,offset asciizb
	push	bx
	call	exstr3
	pop	ax
	jmp	maknum

lisp$subru 'UNASCIIZ',cantcomp,numarg
	mov	bx,ax
	jmp	retuid


lisp$subru 'high-byte',chighbyte,numarg
	mov	al,ah
	mov	ah,0
	jmp	maknum
chighbyte:
	call	cnumar1
	compil	0,<MOV  AL,AH>,'MOV  AL,AH'
cbyte:	compil	0,<MOV  AH,0>,'MOV  AH,0'
	jmp	cmaknum

lisp$subru 'low-byte',clowbyte,numarg
	mov	ah,0
	jmp	maknum
clowbyte:
	call	cnumar1
	jmp	cbyte

lisp$subru 'peek',cpeek,twonum
	mov	es,ax
	mov	ax,es:[bx]
	mov	ah,0
	jmp	maknum
cpeek:	call	cpeek1
	jmp	cbyte

lisp$subru 'peekw',cpeekw,twonum
	mov	es,ax
	mov	ax,es:[bx]
	jmp	maknum
cpeekw:	call	cpeek1
	JMP	CMAKNUM
cpeek1:	call	ctwonu1
	compil	0,<MOV ES,AX>,'MOV  ES,AX'
	compil	0,<MOV AX,ES:[BX]>,'MOV  AX,ES:[BX]'
	ret

lisp$subru 'poke',cpoke
	call	numarg
	push	ax
	call	numarg
	push	ax
	call	numarg
	pop	bx
	pop	es
	mov	es:[bx],al
	jmp	tret
lisp$subru 'pokew',cpokew
	call	numarg
	push	ax
	call	numarg
	push	ax
	call	numarg
	pop	bx
	pop	es
	mov	es:[bx],ax
	jmp	tret
cpoke1:
	car$cdr	a,b,bx
	push	bx
	call	numeriz
	call	push_ax
	pop	bx
	car$cdr	a,b,bx
	push	bx
	call	numeriz
	call	push_ax
	pop	bx
	car	a,bx
	call	numeriz
	compil	0,<POP BX>,'POP  BX'
	compil	0,<POP ES>,'POP  ES'
	RET
cpoke:
	call	cpoke1
	compil  0,<MOV  ES:[BX],AL>,'MOV  ES:[BX],AL'
	RET
cpokew:	
	call	cpoke1
	compil  0,<MOV  ES:[BX],AX>,'MOV  ES:[BX],AX'
	RET

lisp$subru 'input-byte',cnumarg,numarg
	mov	dx,ax
	in	al,dx
	mov	ah,0
	jmp	maknum

lisp$subru 'output-byte',ctwonum,twonum
	mov	dx,ax
	mov	ax,bx
	out	dx,al
	jmp	tret
lisp$subru 'joystick',cnoarg
	mov	dx,201h
	out	dx,al
	mov	bx,0
	mov	di,0
	mov	si,0
joysti1:in	al,dx
	test	al,3
	jz	joysti2
	test	al,1
	jz	x_done
	mov	di,bx
x_done:	test	al,2
	jz	y_done
	mov	si,bx
y_done:	inc	bx
	jmp	joysti1
joysti2:push	di
	mov	ax,si
	call	maknum
	pop	di
	push	ax
	mov	ax,di
	call	maknum
	pop	bx
	jmp	cons

any$reg	macro	onereg,str
	local	gggval
	local   cggval
	LISP$SUBRU str,c&onereg,evalarg
	j$nil	ax,gggval
	call	NUMVAL
	mov	onereg&reg,ax
	jmp	tret
gggval:	mov	ax,onereg&reg
	jmp	maknum
	dataseg	segment
		onereg&reg	dw	0
	dataseg ends
c&onereg:
	j$nil	bx,cggval
	car	a,bx
	call	numeriz
	compil  0,<MOV onereg&reg,AX>,<'MOV [',str,'],AX'>
	ret
cggval:	compil	0,<MOV AX,onereg&reg>,<'MOV AX,[',str,']'>
	jmp	cmaknum
	ret
 endm

any$reg	ax,'AX-reg'
any$reg	bx,'BX-reg'
any$reg	cx,'CX-reg'
any$reg	dx,'DX-reg'
any$reg	ES,'ES-reg'
any$reg ds,'DS-reg'
any$reg	si,'SI-reg'
any$reg	di,'DI-reg'
purge any$reg

LISP$subru 'INT-',CNUMARG,NUMARG
	mov     byte ptr cs:[intarg],al
	mov	byte ptr cs:[intarg-1],11001101b ; INT
ldaregs:mov	ax,[axreg]
	mov	bx,[bxreg]
	mov	cx,[cxreg]
	mov	dx,[dxreg]
	mov	es,[esreg]
	mov	di,[direg]
	mov	si,[sireg]
	mov	ds,[dsreg]
	ret
intarg:	db	0
setregs:mov	ds,cs:[cs_dataseg]
	mov	[dsreg],ds
	mov	[axreg],ax
	mov	[bxreg],bx
	mov	[cxreg],cx
	mov	[dxreg],dx
	mov	[esreg],es
	mov	[direg],di
	mov	[sireg],si
	mov	byte ptr cs:[intarg-1],0C3H ; ret
	test$resu	nc


DATASEG SEGMENT
	BPreg dw 0
DATASEG ENDS

LISP$SUBRU 'lisp-DS',cnoarg
	mov	ax,ds
	jmp	maknum
LISP$SUBRU 'lisp-CS',cnoarg
	mov	ax,cs
	jmp	maknum

lisp$subru 'plm-call-far',cplmcall
	mov	[BPREG],SP
	CALL	NUMARG
	MOV	CS:[PLMSEG],AX
	CALL	NUMARG
	MOV	CS:[PLMOFF],AX
plMARG:	call	evalarg
	j$nil	ax,nomorearg
	call	NUMVAL
	push	AX
	jmp	plmarg
NOMOREARG:
	MOV	BP,[BPREG]
	db 9AH ; CALL FAR
	plmoff	dw 0
	plmseg  dw 0
	jmp	SETREGS

eval_C_num:
	car$cdr	a,b,bx
	push	bx
	call	eval
	call	numval
	pop	bx
	ret

pus_nums:
	R$NIL	BX
	car$cdr	a,b,bx
	push	bx
	call	numeriz
	call	push_ax
	pop	bx
	jmp	pus_nums

cplmcall:
	push	bx
	compil	0,<mov	[BPREG],SP>,'MOV  [BPREG],SP'
	pop	bx
	call	eval_C_num
	MOV	CS:[PLMSEG],AX
	call	eval_c_num
	MOV	CS:[PLMOFF],AX
	call	pus_nums
	compil	0,<MOV	BP,[BPREG]>,'MOV  BP,[BPREG]'
	compil  1,<db 9AH>,'CALL FAR '
	MOV	AX,CS:[PLMOFF]
	call	cword
	comptext '/'
	MOV	AX,CS:[PLMSEG]
	call	cword
	cocall	SETREGS,'SETREGS'
	RET



;================================================
; grafiikka
IFNDEF	DXLISP
dataseg segment
	Egabx dw 0
	color db 1
	x_origo dw 0
	y_origo dw 0
	x_max dw 1000
	y_max dw 500
	X_loc dw 0
	y_LOC DW 0
	X_YLOS	DW 0
	Y_YLOS	DW 0
	delta_x	dw 0
	DELTA_Y	DW 0
	X_APU	DW 0
	Y_APU	DW 0
	MAXI	DW 0
	EPOS	DW 0
	ENEG	DW 0
	E	DW 0
	DISPMODE DB 0
	V90	DW 90
	g_char_mode db 0
dataseg ends

lisp$subru 'g-char-mode',cnumarg,numarg
	Mov	[g_char_mode],al
	JMP	TRET

lisp$subru 'display-mode',cnumarg,numarg
	mov	[dispmode],al
	cmp	al,99
	jz	nokpix3
	CMP	AL,80h
	JZ	HGC
	and	al,7FH
	mov	AH,0
	INT	10H
nokpix3:Mov	[g_char_mode],0
	JMP	TRET

hgc2:	db 9ah
	dw 0
	dw 0
	ret

HGC:	Mov	[g_char_mode],0
	MOV	AX,0B000h
	MOV	ES,AX
	mOV	CX,0FFFFH
	mov	di,0
	mov	al,0
	rep 	stosb

	XOR	AX,AX
	MOV	ES,AX
	LES	SI,ES:[3A8h]
	LES	SI,ES:[SI]
	LES	SI,ES:[SI+34h]
	LES	SI,ES:[SI+4h]
	mov	word ptr cs:[hgc2+1],si
	mov	word ptr cs:[hgc2+3],es
	MOV	AH,21
	MOV	BL,3
	CALL	hgc2
	MOV	AH,29
	call	hgc2
	JMP	TRET

lisp$subru 'color',cnumarg,numarg
	mov	[color],aL
	ret
LISP$SUBRU 'point',ctwonum,twonum
 	mov	[x_loc],ax
	mov	[y_loc],bx
	mov	ax,t
	ret

lisp$subru 'draw',ctwonum,twonum
	mov	cx,1
	mov	[x_ylos],cx
	mov	[y_ylos],cx
	SUB	AX,[X_LOC]
	SUB	BX,[Y_LOC]
	TEST	AX,8000H
	JZ	SK0
	MOV	[X_YLOS],-1
	NEG	AX
SK0:	TEST	BX,8000H
	JZ	SK00
	MOV	[Y_YLOS],-1
	NEG	BX
SK00:	CMP	ax,bx
	jnz	sk000
	cmp	ax,0
	rz
sk000:	MOV	cx,[X_ylos]
	MOV	[X_APU],CX
	MOV	cx,[y_ylos]
	MOV	[y_APU],CX
	SUB	CX,CX
	CMP	AX,BX
	JNC	DXNPDY
	MOV	[X_APU],CX
	MOV	[MAXI],BX
	XCHG	AX,BX
	JMP	EOIF
dxNPDY:	MOV	[Y_APU],CX
	MOV	[MAXI],AX
EOIF:	MOV	[DELTA_X],AX
	MOV	[DELTA_Y],BX
	SUB	BX,AX
	SHL	BX,1
	MOV	[EPOS],BX
	MOV	BX,[DELTA_Y]
	SHL	BX,1
	MOV	[ENEG],BX
	SUB	BX,AX
	MOV	[E],BX
	MOV	CX,[MAXI]
VIIVA:	PUSH	CX
	CALL	SETXYPIX
	MOV	AX,[E]
	MOV	BX,[X_LOC]
	MOV	CX,[Y_LOC]
	TEST	AX,8000H
	JNZ	EP0
	ADD	BX,[X_YLOS]
	ADD	CX,[Y_YLOS]
	ADD	AX,[EPOS]
	JMP	EP02
EP0:	ADD	BX,[X_APU]
	ADD	CX,[Y_APU]
	ADD	AX,[ENEG]
EP02:	MOV	[E],AX
	MOV	[X_LOC],BX
	MOV	[Y_LOC],CX
	POP	CX
	LOOP	VIIVA
	JMP	TRET

lisp$subru 'graph-borders',ctwonum,twonum
	mov	cx,[x_loc]
	mov	[x_origo],cx
	add	ax,cx
	mov	[x_max],ax
	mov	cx,[y_loc]
	mov	[y_origo],cx
	add	bx,cx
	mov	[y_max],bx
	jmp	tret

rajat:
	cmp	cx,[y_origo]
	jc	ohiruu
	cmp	CX,[y_max]
	Jnc	OHIRUU
	cmp	dx,[x_origo]
	jc	ohiruu
	cmp	DX,[x_max]
	jnC	OHIRUU
	ret
ohiruu:	pop	bx
	ret

lisp$subru 'set-pix',ctwonum,twonum
	mov	[x_loc],ax
	mov	[y_loc],bx
	call	setxypix
	jmp	tret
SETXYPIX:
	MOV	DX,[X_LOC]
	MOV	CX,[Y_LOC]
	call	rajat
	MOV	AL,[DISPMODE]
	cmp	al,80h
	jmpn	nz,hgcpix
        MOV	BX,80
        CMP	AL,18
        JMPN	NZ,VGAPIX
        MOV	BX,100
        CMP	AL,88
        JMPN	NZ,VGAPIX
	CMP	AL,5EH
	JMPN	NZ,E5pix
	CMP	AL,6
	JMPN	NZ,HPPIX
	CMP	AL,99
	JMPN	NZ,NOKPIX
	JMP	MUUTPIX


NOKPIX:	mov	ax,0b800H
	mov	es,ax
	mov	bx,0
	jmp	nokp2


HPPIX:	mov	ax,0b800H
	mov	es,ax
	mov	bx,0
	test	cl,1
	jz	even_row
	mov	bx,2000h
even_row:
	shr	cx,1
nokp2:	mov	al,80
	mul	cl
	shr	dx,1
	shr	dx,1
	shr	dx,1
	add	ax,dx
	add	bx,ax
onbyt:	mov	cx,[x_loc]
	not	cx
	and	cl,111b
	mov	ah,es:[bx]
	ror	ah,cl
	mov	al,[color]
	and	al,81H
	test	al,80h
	jz	setcol
	and	al,0FH
	xor	ah,al
	jmp	short xorcol
setcol:	and	ah,11111110b
	or	ah,al
xorcol:	rol	ah,cl
	mov	es:[bx],ah
	ret

VGAALI:	MOV	DX,3CEH
	XCHG	AH,AL
	OUT	DX,AL
        INC	DX
	MOV	AL,AH
        OUT	DX,AL
        RET

VGAPIX:	TEST	[COLOR],80H
	JZ	NORVGA
        MOV	AX,0318H
        CALL	VGAALI
NORVGA:	MOV	AX,0A000H
        MOV	ES,AX
	MOV	AX,0502H
	CALL	VGAALI
	mov	ax,bx
        mul	cX
        MOV	DX,[X_LOC]
	shr	dx,1
	shr	dx,1
	shr	dx,1
	add	ax,dx
	MOV	bx,ax
	mov	cx,[x_loc]
	and	cl,111b
	mov	al,80H
	ror	al,cl
	mov	AH,8
        CALL	VGAALI
        MOV	AL,ES:[BX]
        MOV	AL,[COLOR]
        MOV	ES:[BX],AL
        MOV	AX,08FFH
        CALL	VGAALI
        MOV	AX,0300H
        CALL	VGAALI
        MOV	AX,0500H
        JMP	VGAALI
        
E5pix:	MOV	DX,3CEH
	MOV	AL,9
	OUT	DX,AL
	MOV	AX,[Y_LOC]
	MOV	BX,96
	MOV	DX,0
	IDIV	BX
	MOV	BX,15
	IMUL	BX
	MOV	DX,3CFH
	OUT	DX,AL
	MOV	AX,0A000H
	MOV	ES,AX
	MOV	AX,[Y_LOC]
	MOV	BX,96
	MOV	DX,0
	IDIV	BX
	MOV	AX,640
	IMUL	DX
	ADD	AX,[X_LOC]
	MOV	BX,AX
	MOV	AL,[COLOR]
	MOV	ES:[BX],AL
	JMP	TRET

hgcpix:	MOV	AX,CX
	AND	AX,3
	MOV	AH,AL
	XOR	AL,AL
	SHL	AX,1
	SHL	AX,1
	ADD	AX,0b000H
	MOV	ES,AX
	MOV	AX,CX
	SHR	AX,1
	SHR	AX,1
	IMUL	V90
	MOV	BX,[X_loc]
	SHR	BX,1
	SHR	BX,1
	SHR	BX,1
	ADD	BX,AX
	jmp	onbyt


MUUTPIX:
	XCHG	DX,CX
	MOV	BX,[Egabx]
	MOV	AL,[color]
	MOV	AH,0ch
	INT	10h
	RET

lisp$subru 'ega-bx',cnumarg,numarg
	Mov	[egabx],AX
	jmp 	tret
	
lisp$subru 'graph-region',ctwonum,twonum
recolu:	push	bx
	push	ax
	mov	cx,ax
	push	[x_loc]
rerivi:	push	cx
	call	setxypix
	pop	cx
	inc	[x_loc]
	loop	rerivi
	inc	[y_loc]
	pop	[x_loc]
	pop	ax
	pop	bx
	dec	bx
	jnz	recolu
	jmp	tret

dataseg segment
	kypa db 9
dataseg ends

LISP$SUBRU 'graph-char',cnumarg,numarg
graphc:	cmp	al,20h
	jnc	tavch
	cmp	al,13
	jnz	ehkbs
	mov	[x_loc],10
	add	[y_loc],9
	rc
ehkbs:	cmp	al,8
	rnz
	sub	[x_loc],6
	ret
tavch:	SUB	Al,20h
	IMUL	kypa
	ADD	AX,OFFSET MERKIT
	MOV	BX,AX
	MOV	CX,9
uurivi:	PUSH	BX
	PUSH	CX
	mov	Cx,5
	MOV	AL,[BX]
	SHL	AL,1
	SHL	AL,1
	SHL	AL,1
RRIVI:	PUSH	CX
	PUSH	AX
	TEST	AL,80H
	JZ	NNOTSET
	CALL	SETXYPIX
NNOTSET:
	INC	[X_LOC]
	POP	AX
	SHL	AL,1
	POP	CX
	LOOP	RRIVI
	POP	CX
	pop	Bx
	inc	[y_loc]
	add	[x_loc],-5
	inc	bx
	loop	uurivi
LLOPPU:	add	[x_loc],6
	add	[y_loc],-9
	mov	ax,t
	ret
ENDIF

LISP$SUBRU 'make-fsym',cantcomp,evalarg
makefsym:
	typeof	dl,ax
	cmp	dl,fsymbo
	rz
	push	bx
	push	cx
	cmp	dl,symbol
	jmpn	z,namerr
	car	c,ax
	settype	ax,fsymbo
	mov	bx,[fsymbc]
	rplaca	ax,b
	mov	[bx],cx
	add	bx,2
	mov	[fsymbc],bx
	pop	cx
	pop	bx
	ret


;===============================================
; compileri

dataseg segment
	functype	dw	cantcomp
	PCLOC	DW	FREECODE
	THISADDR	dw	0
	THISCLAUSE	dw	0
	CODEBUG		DB 	0
	RETNUMBER	DB	0
	RETZERO		DB	0
dataseg ends

LISP$SUBRU	'comp-debug',cantcomp,evalarg
	mov	[codebug],al
	ret

lisp$subru 'no-compiled-code',cantcomp,evalarg
	cmp	ax,nil
	jmpn	z,uptohe
	mov	[macdat],ax
	mov	[theseclauses],ax
	mov	[thisclause],ax
	mov	ax,offset freecode
	mov	[pcloc],ax
	mov	word ptr [FSYMBC],offset FSYMBS
	mov	ax,[oblist]
nococo:	cmp	ax,nil	
	rz
	car$cdr	a,b,ax
	push	bx
	typeof	dl,ax
	when	fsymbo,rstosy
	pop	ax
	jmp	nococo
rstosy:	settype	ax,symbol
	car	b
	mov	bx,[bx]
	rplaca	ax,b
	pop	ax
	jmp	nococo

uptohe:	typeof	dl,ax
	cmp	dl,subru
	jmpn	z,nilret
	car	a
	mov	[pcloc],ax
	jmp	tret


lisp$subru 'ncompile',cantcomp,evalarg
cfunc2:
	push	[thisaddr]
	push	[functype]
	push	[thisclause]
	push	ax
	mov	ax,[thisaddr]
	mov	bx,[functype]
	call	cons
	mov	bx,ax
	mov	ax,[thisclause]
	call	cons
	mov	bx,[theseclauses]
	call	cons
	mov	[theseclauses],ax
	pop	ax
	mov	[functype],offset cnoarg
	mov	bx,[pcloc]
	mov	[thisaddr],bx
	mov	[thisclause],ax
	call	ncompi
	mov	ax,offset frame_ret
	call	cjmp
	mov	ax,[thisaddr]
	mov	bx,[functype]
	call	maksbr
	mov	bx,[theseclauses]
	cdr	b,bx
	mov	[theseclauses],bx
	pop	[thisclause]
	pop	[functype]
	pop	[thisaddr]
	ret

spaceforjump:
	mov	cx,[pcloc]
	push	cx
	push	ax
	push	bx
	compil 0,<JMP ERHE>,'JMP  ??	; see below'
	pop	bx
	pop	ax
	pop	cx
	ret
CFUNC1:
	call	spaceforjump
	push	cx
	call	cfunc2
	pop	cx
	push	ax
	call	jmpfrcxhere
	pop	ax
	RET

lisp$subru 'function',cfunction,arg
	ret
cfunction:
	car	a,bx
	CALL	CFUNC1
	jmp	csex

maccod:	mov	di,[pcloc]
	push	ax
	push	bx
	push	di
macco2:	mov	al,cs:[si]
	mov	cs:[Di],al
	inc	si
	inc	di
	loop	macco2
	mov	[pcloc],di
	mov	dl,[codebug]
	cmp	dl,0
	jz	macco3
	call	crlf
	MOV	AX,CS
	CALL	HEXWORD
	PRITHS ':'
	pop	ax
	call	HEXWORD
	PRITHS	':   '
	pop	bx
	CALL	PRINTS
	pop	ax
	ret
macco3:	pop	ax
	pop	ax
	pop	ax
	ret

comerr:	lisp$error ' CODE MEMORY FULL '

cword1:	mov	di,[pcloc]
	cmp	di,0FF00H
	jnc	comerr
	mov	CS:[di],ax
	inc	di
	inc	di
	mov	[pcloc],di
	ret
cword:	call	cword1
	mov	dl,[codebug]
	cmp	dl,0
	rz
	call	HEXWORD
	ret

lisp$subru 'mach-code',cmcode
	push	[pcloc]
	mov	[codebug],0
	mov	ax,[argsta]
	car	b,ax
	call	cmcode
	compil	0,<ret>,'RET'
	pop	[pcloc]
	jmp	[pcloc]

cmcode:	push	bx
	cocall LDAREGS,'LDAREGS, '
	pop	bx
	call	cmc2
	cocall SETREGS,'SETREGS'
	ret

cmc2:	r$nil	bx
	car$cdr	a,b,bx
	car	a,ax
	mov	di,[pcloc]
	mov	CS:[di],al
	inc	di
	mov	[pcloc],di
	mov	dl,[codebug]
	cmp	dl,0
	jz	cmc2
	push	bx
	call	HEXBYTE
	priths ', '
	pop	bx
	jmp	short cmc2

ccall:	push	ax
	mov	bx,[pcloc]
	push	bx
	compil	1,<call EVAL>,'CALL '
reladr:	pop	bx
	add	bx,3
	pop	ax
	push	ax
	sub	ax,bx
	call	cword1
	pop	ax
	mov	dl,[codebug]
	cmp	dl,0
	rz
	jmp	hexword
cjmp:	push	ax
	mov	bx,[pcloc]
	push	bx
	compil	1,<jmp EVAL>,'JMP  '
	jmp	reladr

TESTNIL:COMPIL	0,<OR AX,AX>,'OR   AX,AX	; AX=NIL?'
	RET
SKPNIL:	COMPIL	0,<JZ $+5>,'JZ   $+5'
	ret
SKPT:	COMPIL	0,<JNZ $+5>,'JNZ  $+5'
	ret

SKPTARG:	
	call	ncompi_type
	mov	al,[retzero]
	mov	[retzero],0
	cmp	al,0
	jnz	skpnil
	call	testnil
	jmp	skpt
SKPNILARG:	
	call	ncompi_type
	mov	al,[retzero]
	mov	[retzero],0
	cmp	al,0
	jnz	skpt
	call	testnil
	jmp	skpnil

CREPEUN:MOV	CX,[PCLOC]
	PUSH	CX
	CAR$CDR	A,B,BX
	PUSH	BX
	call	skpnilarg
COREP2:	POP	BX
	call	spaceforjump
	PUSH	CX
	CALL	COMPPROGN
	POP	CX
	POP	AX
	PUSH	CX
	CALL	CJMP
	POP	CX
JMPFRCXHERE:
	MOV	AX,[PCLOC]
	PUSH	AX
	MOV	[PCLOC],CX
	CALL	cjmp
	pop	AX
	MOV	[PCLOC],aX
	RET
CWHILE:	MOV	CX,[PCLOC]
	PUSH	CX
	CAR$CDR	A,B,BX
	PUSH	BX
	call	skptarg
	JMP	COREP2
CREPEAT:mov	ax,[pcloc]
	push	ax
	call	compprogn
	call	testnil
	call	skpt
	pop	ax
	jmp	cjmp
CREPEATTIMES:
	PUSH	BX
	CALL	CNUMAR1
	call	push_AX
	MOV	AX,[PCLOC]
	PUSH	AX
	call	POP_AX
	CALL	TESTNIL
	CALL	SKPT
	call	spaceforjump
	push	cx
	COMPIL	0,<DEC AX>,'DEC  AX'
	call	push_AX
	MOV	BP,SP
	MOV	BX,[BP+4] 
	CDR	B,BX
	CALL	COMPPROGN
	MOV	BP,SP
	MOV	AX,[BP+2]
	CALL	CJMP
	POP	CX
	CALL	JMPFRCXHERE
	POP	AX
	POP	AX
	RET
CIF:	CAR$CDR	A,B,BX
	PUSH	BX
	CALL	SKPTarg
	POP	bx
	call	spaceforjump
	push	cx
	CAR$CDR	A,B,BX
	PUSH	BX
	CALL	NCOMPI
	POP	BX
	call	spaceforjump
	push	cx
	CAR	A,BX
	PUSH	AX
	MOV	BP,SP
	MOV	CX,[BP+4]
	CALL	JMPFRCXHERE
	POP	AX
	CALL	NCOMPI
	POP	CX
	CALL	JMPFRCXHERE
	POP	CX
	RET

CCOND:	PUSH	BX
	mov	cx,[pcloc]	
	push	cx
	add	cx,3
	push	cx
	add	cx,3
	mov	[pcloc],cx
ccondl:	MOV	BP,SP
	MOV	CX,[BP+2]
	call	jmpfrcxhere
	mov	ax,[bp+4]
	j$nil	ax,ccondend
	CAR$CDR	A,B,ax
	MOV	[BP+4],BX
	CAR$CDR	A,B,Ax
	cmp	ax,t
	jz	coelse
	PUSH	BX
	j$nil	bx,rettest
	call	skptarg
	jmp	norett
rettest:call	ncompi
	call	testnil
	CALL	SKPT
norett:	pop	bx
	mov	ax,[pcloc]
	mov	bp,sp
	mov	[bp+2],ax
	add	ax,3
	mov	[pcloc],ax
	j$nil	bx,eijama1
	call	compprogn
eijama1:pop	ax
	push	ax
	call	cjmp
	jmp	ccondl
coelse:	j$nil	bx,ccondend
	call	compprogn
ccondend:
	pop	cx
	call	jmpfrcxhere
	pop	ax
	pop	ax
	ret

STACKARG:
	typeof	dl,AX
	cmp	dl,fsymbo
	jnz	ncompi_pus
	call	pusvar
	ret
NCOMPI_PUS:
	call	NCOMPI
push_AX:compil	0,<PUSH AX>,'PUSH AX'
	RET

NCOMPI:	call	NCOMPI_TYPE
	cmp	[retnumber],0
	jnz	ncompn
	cmp	[retzero],0
	jnz	ncompb
	ret
ncompb:	mov	[retzero],0
	compil	0,<MOV  AX,1>,'MOV  AX,1'
	compil  0,<JZ $+3>,'JZ   $+3'
	compil  0,<DEC AX>,'DEC  AX'
	ret
ncompn:	mov	[retnumber],0
	cocall	maknum,'MAKNUM'
	ret

dataseg segment
	original dw 0 ; for cantcomp
dataseg ends

NCOMPI_TYPE:
	j$ATOM	AX,ATOMCOMP
	mov	[original],ax
	car$cdr	a,b,ax
	typeof	dl,AX
	when	quote,coquot
	when	symbol,contco
	when	fsymbo,contco
	car	d,ax
	cmp	dx,FUNCTIONS
	jmpn	z,CANTCOMP
	push	bx ; ((function ?
	push	ax
	call	eval
	call	cfunc1
	mov	cx,ax ; subru?
	pop	ax
	pop	bx
	jmp	contc2
contco:	PUSH	BX
	PUSH	AX
	CALL	EVAL
	MOV	CX,AX
	POP	AX
	POP	BX
	j$NIL	CX,CANTCOMP
contc2:	mov	dx,[thisclause]
	cmp	dx,cx
	jmpn	nz,recursive
	PUSH	BX
	PUSH	AX
	PUSH	CX
	MOV	AX,CX
	MOV	BX,[theseclauses]
	CALL	ASSOC
	JN$NIL	AX,recursive2
	POP	CX
	POP	AX
	POP	BX
	TYPEOF	DL,cX
	when	subru,compsu
	jmp	CANTCOMP
recursive:
	push	bx
	mov	ax,[thisaddr]
	mov	bx,[functype]
	call	cons
	pop	bx
	mov	cx,[functype]
	push	cx
	ret
recursive2:
	POP	CX
	POP	CX
	cdr	a,ax
	cdr	c,ax
	pop	bx
	push	cx
	ret
compsu:	mov	ax,cx
	cdr	c,cx
	push	cx
	ret
coquot:	car	a,bx
	jmp	csex	
ATOMCOMP:
	typeof	dl,ax
	when	symbol,cosymb
	when	fsymbo,cosymb
csex:	call	saveimm
	push	ax
	call	cconst
	pop	ax
	mov	dl,[codebug]
	cmp	dl,0
	rz
	priths '	; S-object '
	jmp	print

lisp$subru 'macdat',cantcomp
	mov	ax,[macdat]
	ret

saveimm:
	cmp	ax,200H
	rc
	push	ax
	JMP	SAVEIM2
	mov	bx,[macdat]
	call	memeq
	j$nil	ax,saveim2
	pop	ax
	ret
saveim2:pop	ax
        push	ax
	mov	bx,[macdat]
	call	cons
	mov	[macdat],ax
	pop	ax
	ret

cconst:	compil	1,<mov ax,0>,'MOV  AX,'
	call	cword
	ret
cbonst:	compil	1,<mov bx,0>,'MOV  BX,'
	call	cword
	ret
mvtoim:	compil	1,<mov [trace],ax>,'MOV  ['
	call	cword
	comptext '],AX'
	ret
mvtbim:	compil	2,<mov  [trace],bx>,'MOV  ['
	call	cword
	comptext '],BX'
	ret
mvtcim:	compil	2,<mov  [trace],cx>,'MOV  ['
	call	cword
	comptext '],CX'
	ret
mvtdim:	compil	2,<mov  [trace],dx>,'MOV  ['
	call	cword
	comptext '],DX'
	ret

VALOFSYMB:
	car	a,ax
	ret
cosymb:	call	makefsym
	compil	1,<MOV AX,[trace]>,'MOV  AX,['
	car	a,ax
	call	cword
	comptext ']'
	ret

coeval:
	call	csex
	cocall	eval,'EVAL'
	ret

CANTCOMP:
	mov	ax,[original]
	JMP	COEVAL

CNOARG:	car	a,ax ; eval-osote
	call	ccall
	ret
CONEARG:
	car	a,ax
	add	ax,3
	push	ax
	car	a,bx
	call	ncompi
	POP	AX
	CALL	CCALL
	RET

ONE_OF_M:
	car$cdr	a,b,bx
	push	bx
	call	stackarg
	pop	bx
	ret

CTWOARG:
	car	a,ax
	add	ax,3
	push	ax
	call	ctwoar1
	POP	AX
	CALL	CCALL
	RET
CTWOAR1:
	call	one_of_m
	car	a,bx
	CALL	NCOMPI
	call	mov_bx_ax
POP_AX:	COMPIL 	0,<POP ax>,'POP  AX'
	RET



CTREARG:
	car	a,ax
	add	ax,3
	push	ax
	call	one_of_m
	call	one_of_m
	car	a,bx
	CALL	NCOMPI
	compil	0,<mov cx,ax>,'MOV  CX,AX'
cont3arg:
	COMPIL 	0,<POP Bx>,'POP  BX'
	call	pop_ax
	POP	AX
	CALL	CCALL
	RET


CFOUARG:
	car	a,ax
	add	ax,3
	push	ax
	call	one_of_m
	call	one_of_m
	call	one_of_m
	car	a,bx
	CALL	NCOMPI
	compil	0,<mov dx,ax>,'MOV  DX,AX'
	COMPIL 	0,<POP Cx>,'POP  CX'
	jmp	cont3arg

CARGLIST:
	push	ax
	mov	ax,bx
	CALL	clist
	pop	ax
	car	a,ax
	add	ax,3
	jmp	ccall

CNUMAR1:
	car	a,ax
	add	ax,3
	push	ax
	car	a,bx
	CALL	NUMERIZ
	POP	AX
	RET
CNUMARG:CALL	CNUMAR1
	CALL	CCALL
	RET
CTWONU1:car	a,ax
	add	ax,3
	push	ax
	car$cdr	a,b,bx
	typeof	dl,ax
	cmp	dl,number
	jz	dontpush
	PUSH	BX
	call	nUMERIZ
	pop	bx
	call	seconum
	POP	AX
	RET

dontpush:
	push	ax
	call	seconu1
	pop	ax
	call	numeriz
	pop	ax
	ret
seconu1:
	car	a,bx
	typeof	dl,ax
	cmp	dl,number
	jz	intobx
	CALL	NUMERIZ
	call	mov_bx_ax
	ret
seconum:
	car	a,bx
	typeof	dl,ax
	cmp	dl,number
	jz	intobx
	push	ax
	call	push_AX
	pop	ax
	call	seconu1
	call	POP_AX
	ret

intobx:	push	ax
	compil 	1,<MOV BX,0>,'MOV  BX,'
	pop	ax
	car	a,ax
	jmp	cword

NUMERIZ:
	TYPEOF	DL,AX
	WHEN	NUMBER,immnum
	call	ncompi_type
	mov	al,[retnumber]
	mov	[retnumber],0
	cmp	al,0
	rnz
	COCALL	NUMVAL,'NUMVAL'
	RET
immnum:	car	a
	jmp	cconst

CTWONUM:CALL	CTWONU1
	CALL	CCALL
	RET

MOV_BX_AX:
	compil	0,<mov bx,ax>,'MOV  BX,AX'
	ret

HEXWORD:
	mov	dl,'$'
	call	printc
	PUSH	AX
	MOV	AL,aH
	cmp	al,0
	jz	ybyte
	CALL	HEXBYTE
ybyte:	POP	AX
HEXBYTE:PUSH	AX
	ROR	AL,1
	ROR	AL,1
	ROR	AL,1
	ROR	AL,1
	CALL	NIBBLE
	POP	AX
nibble:	and	al,0fh
	cmp	AL,0AH
	JC	DECIMAL
	ADD	AL,'A'-0AH
	JMP	HCHAR
DECIMAL:	
	ADD	AL,'0'
HCHAR:	MOV	DL,AL
	JMP	PRINTC

	
;===============================================
IFNDEF	DXLISP
dataseg segment
	parameters dw 7 dup(0)
	keep_sp	dw 0
	spawn_name db 32 dup(0)
	command_name db 32 dup(0)
	com_par db 64 dup(0)
dataseg ends

lisp$subru 'nspawn',cantcomp,evalarg
	mov	bx,[lastmem]
	j$nil	ax,nspawn
	cmp	[EMS_ON],0
	JMPn	Z,nspawn
	cdr	a,ax
	mov	bx,offset spawn_name
	call	exstr3
	call	saveima
	mov	bx,[nodtyp]
	call	nspawn
	mov	ax,[espsp]
	mov	es,ax
	mov	bx,[lastmem]
	sub	bx,ax
	msdos	4ah
	jmpn	nc,stbler
	jmp	loadima

nspawn:	push	bx
	call	strarg
	mov	bx,offset command_name
	call	exstr3
	call	strarg
	mov	bx,offset com_par+2
	call	exstr3
	cmp	byte ptr [bx-1],0
	jnz	paril
	dec 	bx
	dec	cl
paril:	mov	byte ptr [bx],13
	inc	cl
	mov	[com_par],cl
	mov	byte ptr [com_par+1],020h
	mov	ax,offset com_par
	mov	[parameters+2],ax
	mov	ax,cs:[cs_dataseg]
	mov	[parameters+4],ax
	mov	es,[espsp]
	mov	ax,es:[2ch]
	mov	[parameters],ax ; ????
;
	pop	bx
	mov	ax,es
	sub	bx,ax
	msdos	4ah
	jc	stbler
	mov	ax,cs:[cs_dataseg]
	mov	es,ax
	mov	bx,offset parameters
	mov	dx,offset command_name
	mov	keep_sp,sp
	mov	al,0
	msdos	4bh
	jc	execer
	mov	bx,cs:[cs_dataseg]
	mov	ds,bx
	mov	bx,cs:[cs_stackseg]
	mov	ss,bx
	mov	sp,keep_sp
	call	ems_reset
	jmp	tret

stbler:	call	maknum
	lisp$error ' SETBLOCK error = '
execer: call	maknum
	lisp$error ' EXEC error = '

savim1	macro segment
	pop	bx
	push	bx
	mov	ax,[segment]
	call	savim2
	endm

saveima:
	mov	dx,offset spawn_name
	mov	cx,0
	msdos	3ch
	jmpn	nc,creerr
	push	ax
	savim1	NODtYP
	savim1	CARLOW
	savim1	CARHIG
	savim1	CDRLOW
	savim1	CDRHIG
	pop	ax
	jmp	close

savim2:	mov	cx,[lastnode]
	mov	ds,ax
	mov	dx,0
	msdos	40h
	mov	ax,cs:[cs_dataseg]
	mov	ds,ax
	jmpn	nc,ioerr
	ret

loaim1	macro segment
	pop	bx
	push	bx
	mov	ax,[segment]
	call	loaim2
	endm

loadima:mov	dx,offset spawn_name
	mov	al,0
	msdos	3dh
	jmpn	nc,creerr
	push	ax
	loaim1	NODTYP
	loaim1	CARLOW
	loaim1	CARHIG
	loaim1	CDRLOW
	loaim1	CDRHIG
	pop	ax
	jmp	close

loaim2:	mov	cx,[lastnode]
	mov	ds,ax
	mov	dx,0
	msdos	3fh
	mov	ax,cs:[cs_dataseg]
	mov	ds,ax
	jmpn	nc,ioerr
	ret


dataseg segment
	ds_exeseg dw exeseg
	ds_rapaseg dw rapaseg
dataseg ends

	cs_dataseg dw dataseg
	cs_stackseg dw stackseg

lisp$subru 'continue-in',cantcomp
	cmp	[EMS_ON],0
	JMPN	Z,NILRET
	mov	es,[ds_exeseg]
	mov	ax,sp
	mov	es:[10H],ax
	mov	ax,stackseg
	sub	ax,rapaseg
	mov	es:[0eH],ax

	mov	ax,[lastmem]
	mov	bx,[ds_rapaseg]
	sub	ax,bx
	mov	cl,5
	shr	ax,cl
	add	ax,4
	mov	es:[4],ax

	push	ax	; length by 512
	call	strarg
	call	create
	call	numval
	mov	bx,ax
	push	bx
	push	ds
	mov	ds,[ds_exeseg]
	mov	cx,20H
	mov	dx,0
	msdos	40h
	pop	ds
	pop	bx
	jc	ioerr2
	pop	cx	; length
	mov	ds,[ds_rapaseg]
coco2:	push	ds
	push	cx
	push	bx
	mov	cx,512
	mov	dx,0
	msdos	40h
	pop	bx
	pop	cx
	pop	ds
	jc	ioerr2
	dec	cx
	jz	coco3
	mov	ax,ds
	add	ax,(512/16)
	mov	ds,ax
	jmp	coco2
ioerr2:
	mov	bx,cs:[cs_dataseg]
	mov	ds,bx
	jmp	ioerr

coco3:	mov	ax,cs:[cs_dataseg]
	mov	ds,ax
	mov	ax,bx
	jmp	close


continue_in:
	push	cx
	call	set_interrupts
	pop	ax
	jmp	maknum

ENDIF

lisp$subru 'error-reset',conearg,onearg
	lisp$error 'BREAK '

breikki:;sti
	;mov	dx,cs:[cs_dataseg]
	;mov	ds,dx
	mov	ax,0	
	lisp$error	' **BREAK** '
diverr:	mov	ax,0
	lisp$error ' DIVIDE BY ZERO '
dataseg segment
	this_trace dw 0
dataseg ends

LISP$SUBRU 'err-set',cantcomp
	frame	erhe3
	mov	[trace],nil
	call	evalarg
	mov	sp,[stackmark]
	pop	[stackmark]
	pop	cx
	mov	bx,nil
	jmp	cons

ERHE:	mov	bx,[trace]
	mov	[this_trace],bx
	MOV	BYTE PTR CS:[FRAME_RET],90H ; NOP
	call	ems_reset
	jmp	framexit

ERHE3:	MOV	BYTE PTR CS:[FRAME_RET],0C3H ; RET
	call	printer
	jmp	nilret

ERHE2:	MOV	BYTE PTR CS:[FRAME_RET],0C3H ; RET
	mov	ds,cs:[cs_dataseg]
	mov	[dsreg],ds
	mov	byte ptr cs:[intarg-1],0C3H ; ret
ifdef	dxlisp
	mov	sp,NEW_DX_SP
else
	MOV	SP,offset topofstack
endif
	frame	erhe2
	call	printer
IFNDEF	DXLISP
	mov	bx,4
CLOSEA:	PUSH	BX
	msdos	3eH
	POP	BX
	INC	BX
	CMP	BX,20
	JNZ	CLOSEA
ENDIF
	mov	ax,nil
	mov	[argsta],ax
	mov	[enviro],ax
	mov	ax,[autost]
	call	eval
	call	eval
	jmp	looppi

PRINTER:mov	cx,0
	mov	[inchn],cx
	mov	[outchn],cx
	mov	[nxtch],cl
	PUSH	AX
	call	crlf
	priths 'ERROR, '
	mov	bx,[erheen_syy]
	CALL	PRINTS
	POP	AX
	cmp	[throwtag],nil
	jz	eithrow
	mov	ax,[throwtag]
	mov	[throwtag],nil
eithrow:CALL	PRINT
	call	crlf
	mov	ax,[this_trace]
	j$nil	ax,nrstrd
	priths	'Trace:'
	mov	ax,[this_trace]
	mov	[this_trace],nil
	call	pprint
nrstrd:
	RET

set_interrupts:
		IFNDEF	DXLISP
	mov	ax,cs
	mov	ds,ax
	mov	dx,offset breikki
	mov	al,23h
	msdos	25h
	mov	dx,offset diverr
	mov	al,0
	msdos	25h
		endif
set_i2:	MOV	AX,cs:[cs_dataseg]
	MOV	DS,AX
	mov	[dsreg],ax
	ret


LISP:	MOV	AX,cs:[cs_dataseg]
	MOV	DS,AX
		IFDEF DXLISP
		dataseg segment
			SS_jemma dw 0
			SP_jemma dw 0
		dataseg ends
		MOV	[SS_JEMMA],ss
		MOV	[SP_JEMMA],sp
		MOV	SP,NEW_DX_SP
		mov	dl,'%'
		call	printc
		priths	<'Tyhjaa',13,10>
		mov	al,[dx_exit]
		cmp	al,0
		jmpn	z,looppi
		ELSE
	mov	[espsp],es
 	mov	ax,0
	mov	[inchn],ax
	mov	[outchn],ax
	MOV	ax,cs:[cs_stackseg]
	mov	ss,ax
	MOV	SP,offset topofstack
		ENDIF
	call	setnodseg
	CALL	RESETMEM
	call	tarpeelliset
	MOV	DL,0
	mov	[nxtch],dl
	priths 'Welcome to NOKOLISP-86, ('
	mov	ax,[lastnode]
	call	hexword
	priths <' nodes free)',13,10>
	call	set_interrupts
	frame	erhe2
	mov	ax,oblist
	call	print
looppi:	IFDEF	DXLISP
	call	crlf
	priths  'NOK> '
	ENDIF
 	call	read
	call	eval
	push	ax
	IFDEF	DXLISP
	call	crlf
	ELSE
	priths  ' '
	ENDIF
	pop	ax
	call	pprint
	jmp     looppi

FREECODE:

SETNODSEG:
		IFNDEF	DXLISP
	mov	ax,3FF0H
		ELSE
		MOV	AX,07FF0H
		ENDIF
	mov	[lastnode],ax
setnods2:
	shr	ax,1
	shr	ax,1
	shr	ax,1
	shr	ax,1
	mov	bx,codeseg
	add	bx,1000h
	mov	[nodtyp],bx
	add	bx,ax
	mov	[carlow],bx
	add	bx,ax
	mov	[carhig],bx
	add	bx,ax
	mov	[cdrlow],bx
	add	bx,ax
	mov	[cdrhig],bx
	add	bx,ax
	mov	[lastmem],bx
	ret

RESETSUBR:
 	R$NIL	BX
	PUSH	BX
	MOV	ax,[BX+2]
	mov	bx,[bx+4]
	CALL	MAKSBR
	POP	BX
	PUSH	BX
	ADD	BX,6
	CALL	oblnam
	POP	BX
	MOV	BX,[BX]
	JMP	RESETSUBR




RESETMEM:
 	MOV	ax,0
	MOV	BX,NIL
RSM3:	SETTYPE	AX,aL
	rplaca	ax,b
	rplacd	ax,b
	INC	AX
	CMP	AX,fstnode
	JNZ	RSM3
	MOV	DL,GARBA
RSM2:	SETTYPE	AX,DL
	INC	AX
	CMP	AX,[lastnode]
	JNZ	RSM2
	mov	ax,ZERONUM
RSM4:	settype	ax,number
	mov	BX,AX
	SUB	BX,ZERONUM
	rplaca	ax,b
	inc	ax
	cmp	ax,fstnode
	jnz	rsm4
	MOV	BX,offset NXTSBR
	CALL	RESETSUBR
	RET

tarpeel	macro	nimi,str
dataseg	segment
	tempaddr = $
	  db	str,0
	nimi dw 0
dataseg	ends
	mov	ax,0
	mov	bx,offset tempaddr
	call	retuid
	mov	[nimi],ax
	endm
tarpeelliset:
	tarpeel	quotes,'quote'
	rplaca	ax,a
	tarpeel	trues,'t'
	rplaca	ax,a
	tarpeel functions,'function'
	tarpeel	lambda,'lambda'
	tarpeel	autost,'autost'
	rplaca	ax,a
	ret


CODESEG ENDS

IFNDEF	DXLISP
dataseg segment
MERKIT db 0
db 0
db 0
db 0
db 0
db 0
db 0
db 0
db 0

db 0
db 00100b
db 00100b
db 00100b
db 00100b
db 0
db 00100b
db 0
db 0

db 0
db 01010b
db 01010b
db 0
db 0
db 0
db 0
db 0
db 0
;#
db 0
db 01010b
db 01010b
db 11111b
db 01010b
db 11111b
db 01010b
db 01010b
db 0
;$
db 0
db 00100b
db 01111b
db 10100b
db 01110b
db 00101b
db 11110b
db 00100b
db 0
;%
db 0
db 11000b
db 11001b
db 00010b
db 00100b
db 01000b
db 10011b
db 00011b
db 0
;&
db 0
db 01000b
db 10100b
db 01000b
db 10101b
db 10011b
db 01101b
db 0
db 0

;'
db 0
db 00100b
db 01000b
db 0
db 0
db 0
db 0
db 0
db 0
;(
db 0
db 00010b
db 00100b
db 01000b
db 01000b
db 01000b
db 00100b
db 00010b
db 0

db 0
db 01000b
db 00100b
db 00010b
db 00010b
db 00010b
db 00100b
db 01000b
db 0
;*
db 0
db 00100b
db 10101b
db 01110b
db 00100b
db 01110b
db 10101b
db 00100b
db 0

;+
db 0
db 0
db 00100b
db 00100b
db 11111b
db 00100b
db 00100b
db 0
db 0
;A,
db 0
db 0
db 0
db 00000b
db 00000b
db 01100b
db 01100b
db 00100b
db 01000b

db 0
db 0
db 0
db 11111b
db 0
db 0
db 0
db 0
db 0
;. 
db 0
db 0
db 0
db 0
db 0
db 01100b
db 01100b
db 0
db 0
;/
db 00000b
db 00001b
db 00010b
db 00100b
db 01000b
db 10000b
db 0
db 0
db 0
;0
db 0
db 01110b
db 10011b
db 10101b
db 11001b
db 10001b
db 01110b
db 0
db 0
;1
db 0
db 00100b
db 01100b
db 00100b
db 00100b
db 00100b
db 01110b
db 0
db 0
;2
db 0
db 01110b
db 10001b
db 00001b
db 00110b
db 01000b
db 11111b
db 0
db 0
;3
db 0
db 01111b
db 00010b
db 00110b
db 00001b
db 10001b
db 01110b
db 0
db 0
;4
db 0
db 00010b
db 00110b
db 01010b
db 11111b
db 00010b
db 00010b
db 0
db 0
;5
db 0
db 01111b
db 01000b
db 11110b
db 00001b
db 10001b
db 01110b
db 0
db 0
;6
db 0
db 00110b
db 01000b
db 10110b
db 11001b
db 10001b
db 01110b
db 0
db 0

db 0
db 11111b
db 00010b
db 00100b
db 01000b
db 01000b
db 01000b
db 0
db 0

db 0
db 01110b
db 10001b
db 01110b
db 10001b
db 10001b
db 01110b
db 0
db 0
;98
db 0
db 01110b
db 10001b
db 10001b
db 01111b
db 00010b
db 01100b
db 0
db 0
;:
db 0
db 0
db 01100b
db 01100b
db 0
db 01100b
db 01100b
db 0
db 0

db 0
db 0
db 01100b
db 01100b
db 0
db 01100b
db 01100b
db 00100b
db 01000b
;<
db 0
db 0001b
db 0010b
db 0100b
db 1000b
db 0100b
db 0010b
db 0001b
db 0

db 0
db 0
db 11111b
db 0
db 11111b
db 0
db 0
db 0
db 0

db 0
db 01000b
db 00100b
db 00010b
db 00001b
db 00010b
db 00100b
db 01000b
db 0
;?
db 0
db 01110b
db 10001b
db 00010b
db 00100b
db 0
db 00100b
db 0
db 0
;@
db 0
db 01110b
db 10001b
db 10101b
db 10111b
db 10110b
db 10000b
db 01111b
db 0
;A
db 0
db 01110b
db 10001b
db 10001b
db 11111b
db 10001b
db 10001b
db 0
db 0

db 0
db 11110b
db 10001b
db 11110b
db 10001b
db 10001b
db 11110b
db 0
db 0

db 0
db 01110b
db 10001b
db 10000b
db 10000b
db 10001b
db 01110b
db 0
db 0

db 0
db 11110b
db 10001b
db 10001b
db 10001b
db 10001b
db 11110b
db 0
db 0

db 0
db 11111b
db 10000b
db 11100b
db 10000b
db 10000b
db 11111b
db 0
db 0

db 0
db 11111b
db 10000b
db 11100b
db 10000b
db 10000b
db 10000b
db 0
db 0

db 0
db 01110b
db 10001b
db 10000b
db 10011b
db 10001b
db 01110b
db 0
db 0

db 0
db 10001b
db 10001b
db 11111b
db 10001b
db 10001b
db 10001b
db 0
db 0

db 0
db 01110b
db 00100b
db 00100b
db 00100b
db 00100b
db 01110b
db 0
db 0

db 0
db 00111b
db 00010b
db 00010b
db 00010b
db 10010b
db 01100b
db 0
db 0

db 0
db 10001b
db 10010b
db 10100b
db 11000b
db 10100b
db 10011b
db 0
db 0

db 0
db 10000b
db 10000b
db 10000b
db 10000b
db 10000b
db 11111b
db 0
db 0
 
db 0
db 10001b
db 11011b
db 10101b
db 10101b
db 10001b
db 10001b
db 0
db 0

db 0
db 10001b
db 11001b
db 10101b
db 10101b
db 10011b
db 10001b
db 0
db 0

db 0
db 01110b
db 10001b
db 10001b
db 10001b
db 10001b
db 01110b
db 0
db 0
;ABCDEFGHIJKLMNOPQRSTUVXYZ{
db 0
db 11110b
db 10001b
db 10001b
db 11110b
db 10000b
db 10000b
db 0
db 0

db 0
db 01110b
db 10001b
db 10001b
db 10001b
db 10101b
db 01110b
db 00100b
db 0

db 0
db 11110b
db 10001b
db 10001b
db 11110b
db 10010b
db 10001b
db 0
db 0

db 0
db 01110b
db 10000b
db 01110b
db 00001b
db 10001b
db 01110b
db 0
db 0

db 0
db 11111b
db 00100b
db 00100b
db 00100b
db 00100b
db 00100b
db 0
db 0

db 0
db 10001b
db 10001b
db 10001b
db 10001b
db 10001b
db 01110b
db 0
db 0

db 0
db 10001b
db 10001b
db 10001b
db 10001b
db 01010b
db 00100b
db 0
db 0

db 0
db 10001b
db 10001b
db 10001b
db 10101b
db 11011b
db 10001b
db 0
db 0

db 0
db 10001b
db 01010b
db 00100b
db 01010b
db 10001b
db 10001b
db 0
db 0

db 0
db 10001b
db 10001b
db 01110b
db 00100b
db 00100b
db 00100b
db 0
db 0

db 0
db 11111b
db 00010b
db 00100b
db 01000b
db 10000b
db 11111b
db 0
db 0

db 10001b
db 00100b
db 01010b
db 10001b
db 11111b
db 10001b
db 10001b
db 0
db 0

db 10001b
db 01110b
db 10001b
db 10001b
db 10001b
db 10001b
db 01110b
db 0
db 0

db 0
db 01110b
db 00010b
db 00010b
db 00010b
db 00010b
db 01110b
db 0
db 0

db 0
db 00100b
db 01010b
db 10001b
db 00000b
db 00000b
db 00000b
db 0
db 0

db 0
db 0
db 0
db 0
db 0
db 0
db 0
db 11111b
db 0
; !@#$%^&
db 11101b
db 00101b
db 00101b
db 11111b
db 10100b
db 10100b
db 10111b
db 0
db 0
;abcedfghijklmnopqrstuvxyz}{
db 0
db 0
db 0
db 01110b
db 00001b
db 11101b
db 01111b
db 0
db 0

db 0
db 10000b
db 10000b
db 11110b
db 10001b
db 10001b
db 11110b
db 0
db 0
 
db 0
db 0
db 0
db 01110b
db 10000b
db 10001b
db 01110b
db 0
db 0

db 0
db 00001b
db 00001b
db 01111b
db 10001b
db 10001b
db 01111b
db 0
db 0

db 0
db 0
db 0
db 01110b
db 111111b
db 10000b
db 01110b
db 0
db 0

db 0
db 00010b
db 00100b
db 01110b
db 00100b
db 00100b
db 01110b
db 0
db 0

db 0
db 0
db 0
db 01111b
db 10001b
db 10001b
db 01111b
db 00001b
db 01110b

db 0
db 10000b
db 10000b
db 10110b
db 11001b
db 10001b
db 10001b
db 0
db 0

db 0
db 00100b
db 0
db 01100b
db 00100b
db 00100b
db 01110b
db 0
db 0

db 0
db 00010b
db 0
db 00110b
db 00010b
db 00010b
db 00010b
db 00010b
db 01100b


db 0
db 10000b
db 10000b
db 10010b
db 10100b
db 11000b
db 10110b
db 0
db 0

db 0
db 01100b
db 00100b
db 00100b
db 00100b
db 00100b
db 01110b
db 0
db 0

db 0
db 0
db 0
db 11010b
db 10101b
db 10101b
db 10101b
db 0
db 0

db 0
db 0
db 0
db 11110b
db 10001b
db 10001b
db 10001b
db 0
db 0

db 0
db 0
db 0
db 01110b
db 10001b
db 10001b
db 01110b
db 0
db 0

db 0
db 0
db 0
db 11110b
db 10001b
db 10001b
db 11110b
db 10000b
db 10000b

db 0
db 0
db 0
db 01111b
db 10001b
db 10001b
db 01111b
db 00001b
db 00001b


db 0
db 0
db 0
db 10110b
db 11000b
db 10000b
db 10000b
db 0
db 0

db 0
db 0
db 0
db 01110b
db 11100b
db 00011b
db 01110b
db 0
db 0

db 0
db 0100b 
db 0100b
db 1110b
db 0100b
db 0100b
db 0011b
db 0
db 0

db 0
db 0
db 0
db 10001b
db 10001b
db 10001b
db 01111b
db 0
db 0

db 0
db 0
db 0
db 10001b
db 10001b
db 01010b
db 00100b
db 0
db 0

db 0
db 0
db 0
db 10001b
db 10101b
db 11011b
db 10001b
db 0
db 0

db 0
db 0
db 0
db 10001b
db 01010b
db 01010b
db 10001b
db 0
db 0
;y
db 0
db 0
db 0
DB 10001B
db 10001b
db 10001b
db 01111b
db 00001b
db 00110b


db 0
db 0
db 0
db 1111b
db 0010b
db 0100b
db 1111b
db 0
db 0

db 00000b
db 01010b
db 00000b
db 01110b
db 00001b
db 11101b
db 01111b
db 0
db 0


db 0
db 01010b
db 00000b
db 01110b
db 10001b
db 10001b
db 01110b
db 0
db 0

db 0
db 01100b
db 00100b
db 00100b
db 00010b
db 00100b
db 00100b
db 01100b
db 0
;~

db 0
db 00001b
db 01110b
db 10000b
db 0
db 0
db 0
db 0
db 0

dataseg ends
ENDIF


END LISP




