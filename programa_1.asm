; -- header ---
bits 64
default rel
; -- variables --
section .bss
leer_numero resq 1 ; 64-bits int = 8 bytes
; -- constants --
section .data
leer_formato db "%d", 0 ; el formato de string para scanf
string_literal_0 db "not equal", 0
string_literal_1 db "equal", 0
; -- Entry Point --
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
	LEA rcx, leer_formato
	LEA rdx, leer_numero
	XOR eax, eax
	CALL scanf
	PUSH qword [leer_numero]
; -- READ ---
	LEA rcx, leer_formato
	LEA rdx, leer_numero
	XOR eax, eax
	CALL scanf
	PUSH qword [leer_numero]
; -- SUB ---
	POP rax
	SUB qword [rsp], rax
; -- JUMP.EQ.0 ---
	CMP qword [rsp], 0
	JE L1
; -- PRINT ---
	LEA rcx, string_literal_0
	XOR eax, eax
	CALL printf
; -- HALT ---
	JMP EXIT_LABEL
; -- Label ---
	L1:
; -- PRINT ---
	LEA rcx, string_literal_1
	XOR eax, eax
	CALL printf
; -- HALT ---
	JMP EXIT_LABEL
EXIT_LABEL:
	XOR rax, rax
	CALL ExitProcess
