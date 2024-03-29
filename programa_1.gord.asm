; -- header ---
bits 64
default rel
 -- variables --
section .bss
 -- constants --
section .data
 -- Entry Point --
section .text
global main
extern ExitProcess
extern printf
extern scanf

main:
	PUSH rbp
	MOV rbp, rsp
	SUB rsp, 32
; -- READ ---
; TODAVIA SIN IMPLEMENTAR 
; -- READ ---
; TODAVIA SIN IMPLEMENTAR 
; -- SUB ---
	POP rax
	SUB qword [rsp], rax
; -- JUMP.EQ.0

CMP qword [rsp], 0
	JE L1
; -- PRINT ---
; TODAVIA SIN IMPLEMENTAR 
 -- HALT ---
	JMP EXIT_LABEL; -- Label ---
L1:
; -- PRINT ---
; TODAVIA SIN IMPLEMENTAR 
 -- HALT ---
	JMP EXIT_LABELEXIT_lABEL:
	XOR rax, rax
	CALL ExitProcess
