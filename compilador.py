'''
Compilador: GordPiler
---------------------
Agustín Brogliatti
Luciano Esteban
'''
import sys


path_programa = sys.argv[1]

# Lectura de líneas de archivo
lineas_programa = []
with open(path_programa, "r") as archivo_programa:
    lineas_programa = [linea.strip() for linea in archivo_programa.readlines()]


programa = []

# Conversor:

for linea in lineas_programa:
    partes = linea.split(" ")
    codigo_op = partes[0]  # Código de operación

    # Se revisa por líneas vacías
    if codigo_op == "":
        continue

    # Se guarda el código de operación como token
    programa.append(codigo_op)

    # Se maneja cada código de operación

    if codigo_op == "PUSH":
        # Se espera un número
        numero = int(partes[1])
        programa.append(numero)
    elif codigo_op == "PRINT":
        # Parsea un literal a texto (string)
        str_literal = ' '.join(partes[1:])[1:-1]
        programa.append(str_literal)
    elif codigo_op == "JUMP.EQ.0":
        # Lee etiqueta
        etiqueta = partes[1]
        programa.append(etiqueta)
    elif codigo_op == "JUMP.GT.0":
        # Lee etiqueta
        etiqueta = partes[1]
        programa.append(etiqueta)


# Log para testing de tokens revisados
# print(programa)


# Cambiar límite de [:5] a que sea dinámico por punto (esto ofrecería una
# mayor escalabilidad en el cambio de nombre de la extensión del lenguaje)
asm_path = path_programa[:55] + ".asm"
salida = open(asm_path, "w")


'''
Compilación a Assembly
'''

# Acá especificamos que vamos a hacer un programa sistemas de 64 bits
# y que la a tener direccionamiento relativo
salida.write("""; -- header ---
bits 64
default rel
""")

# A partir de este punto se crean 3 secciones distintas en el archivo assembly


# Sección de inicialización de variables
salida.write(""" -- variables --
section .bss
""")


# Sección de inicialización de constantes 
salida.write(""" -- constants --
section .data
""")

# Sección de lógica en Assembly:
salida.write(""" -- Entry Point --
section .text
global main
extern ExitProcess
extern printf
extern scanf

main:
\tPUSH rbp
\tMOV rbp, rsp
\tSUB rsp, 32
""")


ip = 0  # IP: Instrucción puntero (instruction pointer)

while ip < len(programa):
    codigo_op = programa[ip]
    ip += 1

    if codigo_op.endswith(":"):
        salida.write("; -- Label ---\n")
        salida.write(f"{codigo_op}\n")
    elif codigo_op == "PUSH":
        numero = codigo_op[ip]
        ip += 1

        salida.write("; -- PUSH ---\n")
        salida.write(f"\tPUSH {numero}\n")
    elif codigo_op == 'POP':
        salida.write("; -- POP ---\n")
        salida.write("\tPOP\n")
    elif codigo_op == "ADD":
        salida.write(";-- ADD ---\n")
        # Alternativa larga para el comando ADD (la dejamos como curiosidad)
        # salida.write("\nPOP rax\n")
        # salida.write("\nPOP rbx\n")
        # salida.write("\nADD rbx, rax\n")
        # salida.write("\nPUSH rbx\n")
        salida.write("\tPOP rax\n")
        salida.write("\tADD qword [rsp], rax\n")  # qword indica 8 bits
    elif codigo_op == "SUB":
        salida.write("; -- SUB ---\n")
        salida.write("\tPOP rax\n")
        salida.write("\tSUB qword [rsp], rax\n")
    elif codigo_op == "PRINT":
        str_literal_index = programa[ip]
        ip += 1

        salida.write("; -- PRINT ---\n")
        salida.write("; TODAVIA SIN IMPLEMENTAR \n")
    elif codigo_op == "READ":
        salida.write("; -- READ ---\n")
        salida.write("; TODAVIA SIN IMPLEMENTAR \n")
    elif codigo_op == "JUMP.EQ.0":
        etiqueta = programa[ip]
        ip += 1

        salida.write("; -- JUMP.EQ.0\n")
        salida.write("\nCMP qword [rsp], 0\n")
        salida.write(f"\tJE {etiqueta}\n")
    elif codigo_op == "JUMP.GET.0":
        etiqueta = programa[ip]
        ip += 1

        salida.write("; -- JUMP.GT.0 ---\n")
        salida.write("\tCMP qword [rsp], 0\n")
        salida.write(f"\tJG {etiqueta}\n")
    elif codigo_op == "HALT":
        salida.write(" -- HALT ---\n")
        salida.write("\tJMP EXIT_LABEL")


salida.write("EXIT_lABEL:\n")
salida.write("\tXOR rax, rax\n")
salida.write("\tCALL ExitProcess\n")

salida.close()