code	SEGMENT  public
	ORG	100H
	ASSUME	CS:code, DS:code, ES:code

IIK:	JMP	ALKU

VANHA 	DW 0
VANHA2	DW 0


INTTI:
	NOP
	NOP
	PUSHF                              
	cmp	AH,0BH
	jz	NOTOHI
	cmp	AH,0FH
	JZ	NOTOHI
	cmp	AH,0
	JNZ	OHI
NOTOHI: MOV 	AH,80
	MOV	Al,6
OHI:	POPF	                                   
	;JMP	FAR CS:[VANHA]                         
	db 2EH,0FFH,2EH,03H,01H   

ALKU:	MOV	AX,3510H                            
	INT	21H                                 
	MOV	[VANHA],BX                          
	MOV	[VANHA2],ES                          
	MOV	DX,OFFSET INTTI                           
	MOV	AX,2510H                            
	INT	21H                                 
	MOV	DX,OFFSET ALKU                            
	INT	27H                                 



code	ENDS
	END IIK

