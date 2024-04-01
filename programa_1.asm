; -- header ---
bits 64
default rel
; -- variables --
section .bss
leer_numero resq 1 ; 64-bits int = 8 bytes
; -- constants --
section .data
leer_formato db "%d", 0 ; el formato de string para scanf
str_literal_0 db "odd", 0
str_literal_1 db "even", 0
fmt db "Hello, world from assembly!", 0
; -- Entry Point --
section .text
global main
extern exit
extern printf
extern scanf

main:
	PUSH rbp
	MOV rbp, rsp
	SUB rsp, 32
; -- READ ---
; TODAVIA SIN IMPLEMENTAR 
	LEA rcx, leer_formato
	LEA rdx, leer_numero
	XOR eax, eax
	CALL scanf
	PUSH qword [leer_numero]
; -- JUMP.EQ.0 ---
	CMP qword [rsp], 0
	JE L1
; -- Label ---
	LOOP:
; -- PUSH ---
	PUSH 2
; -- SUB ---
	POP rax
	SUB qword [rsp], rax
; -- JUMP.EQ.0 ---
	CMP qword [rsp], 0
	JE L1
; -- JUMP.GT.0 ---
	CMP qword [rsp], 0
	JG LOOP
; -- PRINT ---
	LEA rcx, str_literal_0
	XOR eax, eax
	call printf wrt ..plt
; -- HALT ---
	JMP EXIT_LABEL
; -- Label ---
	L1:
; -- PRINT ---
	LEA rcx, str_literal_1
	XOR eax, eax
	call printf wrt ..plt
; -- HALT ---
	JMP EXIT_LABEL
EXIT_LABEL:
	XOR rax, rax
	CALL exit
