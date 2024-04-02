'''
Compilador: GordPiler
---------------------
Agustín Brogliatti
Luciano Esteban
'''
import sys
import os


# Path del pograma .gord que queremos compilar a .asm.
path_programa = sys.argv[1]

# Revisamos si el script contiene el parametro verbose.
# Verbose corre el script en bash mostrando asi mas detalles.
try:
    if sys.argv[2] == "verbose":
        verbose = True
except:
    verbose = False

# Lectura de líneas de archivo.
lineas_programa = []
with open(path_programa, "r") as archivo_programa:
    lineas_programa = [linea.strip() for linea in archivo_programa.readlines()]

# Aqui vamos a almacenar todas instrucciones del programa en lenguaje .gord
programa = []

# Comandos:

comando_push = "ALMACENAR"
comando_print = "IMPRIMIR"
comando_jump_eq_0 = "SALTAR_SI_0"
comando_jump_gt_0 = "SALTAR_SI_MAYOR_0"
comando_add = "SUMAR"
comando_sub = "RESTAR"
comando_pop = "RETIRAR"
comando_read = "LEER"
comando_halt = "TERMINAR"


# Conversor, formateamos los typos segun cada operación:

for linea in lineas_programa:
    partes = linea.split(" ")  # capturamos los distintos tokens y el valor si es el caso.
    codigo_op = partes[0]  # Código de operación.

    # Se revisa por líneas vacías
    if codigo_op == "":
        continue

    # Se guarda el código de operación como token.
    programa.append(codigo_op)

    # Se maneja cada código de operación.

    if codigo_op == comando_push:
        # Se espera un número junto a este token.
        numero = int(partes[1])
        programa.append(numero)
    elif codigo_op == comando_print:
        # Parsea un literal a texto (string).
        str_literal = ' '.join(partes[1:])[1:-1]
        programa.append(str_literal)
    elif codigo_op == comando_jump_eq_0:
        # Lee etiqueta.
        etiqueta = partes[1]
        programa.append(etiqueta)
    elif codigo_op == comando_jump_gt_0:
        # Lee etiqueta.
        etiqueta = partes[1]
        programa.append(etiqueta)


# Log para testing de tokens revisados.
# print(programa)


'''
Libro que mantiene los literales de string (texto a compilar, lo que se
considera un string por el compilador nuevo).
'''

str_literales = []
for ip in range(len(programa)):
    if programa[ip] == comando_print:
        str_literal = programa[ip + 1]
        programa[ip + 1] = len(str_literales)
        str_literales.append(str_literal)


'''
Compilación a Assembly
'''
# Creamos el archivo .asm.
if not os.path.exists("asm"):
    os.mkdir("asm")


asm_path = os.path.join("asm", path_programa.rsplit('.', 1)[0] + ".asm")
salida = open(asm_path, "w")


# Acá especificamos que vamos a hacer un programa sistemas de 64 bits.
# y que la a tener direccionamiento relativo.
salida.write("""; -- header ---
bits 64
default rel
""")

# A partir de este punto se crean 3 secciones distintas en el archivo assembly.


# Sección de inicialización de variables.
salida.write("""; -- variables --
section .bss
leer_numero resq 1 ; 64-bits int = 8 bytes
""")


# Sección de inicialización de constantes.
salida.write("""; -- constants --
section .data
leer_formato db "%d", 0 ; el formato de string para scanf
""")


# Especificar un "0" al final implica que cada string debe terminar
# con un terminador núlo.
# Especificar "db"significa que cada carácter debe ser guardado en un byte.
for idx, str_literal in enumerate(str_literales):
    salida.write(f"str_literal_{idx} db \"{str_literal}\", 0\n")
# salida.write("fmt db \"Hello, world from assembly!\", 0\n")

# Sección de lógica en Assembly:
salida.write("""; -- Entry Point --
section .text
global main
extern exit
extern printf
extern scanf

main:
\tPUSH rbp
\tMOV rbp, rsp
\tSUB rsp, 64
""")


ip = 0  # IP: Instrucción puntero (instruction pointer).

# En este while comenzamos a escribir las instrucciones en .asm.
while ip < len(programa):
    codigo_op = programa[ip]
    ip += 1
    if codigo_op.endswith(":"):
        salida.write("; -- Label ---\n")
        salida.write(f"\t{codigo_op}\n")
    elif codigo_op == comando_push:
        numero = programa[ip]
        ip += 1
        salida.write("; -- PUSH ---\n")
        salida.write(f"\tPUSH {numero}\n")
    elif codigo_op == comando_pop:
        salida.write("; -- POP ---\n")
        salida.write("\tPOP\n")
    elif codigo_op == comando_add:
        salida.write("; -- ADD ---\n")
        # Alternativa larga para el comando ADD (la dejamos como curiosidad).
        # salida.write("\nPOP rax\n")
        # salida.write("\nPOP rbx\n")
        # salida.write("\nADD rbx, rax\n")
        # salida.write("\nPUSH rbx\n")
        salida.write("\tPOP rax\n")  # Extrae el primer valor de la pila a rax
        salida.write("\tADD qword [rsp], rax\n")
        # Suma el valor extraído al valor en la cima de la pila.
        # qword indica 8 bits
    elif codigo_op == comando_sub:
        salida.write("; -- SUB ---\n")
        salida.write("\tPOP rax\n")  # Extrae el primer valor de la pila a rax.
        salida.write("\tSUB qword [rsp], rax\n")  # Resta el valor extraído al valor en la cima de la pila.
    elif codigo_op == comando_print:
        str_literal_index = programa[ip]
        ip += 1
        salida.write("; -- PRINT ---\n")
        # Carga la dirección del literal de cadena a rcx.
        salida.write(f"\tLEA rcx, str_literal_{str_literal_index}\n")
        salida.write("\tXOR eax, eax\n")  # Limpia eax (registro de retorno de función).
        salida.write("\tCALL printf\n")  # Llama a la función printf para imprimir la cadena.
    elif codigo_op == comando_read:
        salida.write("; -- READ ---\n")
        salida.write("\tLEA rcx, leer_formato\n")
        salida.write("\tLEA rdx, leer_numero\n")
        salida.write("\tXOR eax, eax\n")
        salida.write("\tCALL scanf\n")
        salida.write("\tPUSH qword [leer_numero]\n")
    elif codigo_op == comando_jump_eq_0:
        etiqueta = programa[ip]
        ip += 1
        salida.write("; -- JUMP.EQ.0 ---\n") 
        salida.write("\tCMP qword [rsp], 0\n")  # Compara el valor en la cima de la pila con cero.
        salida.write(f"\tJE {etiqueta}\n")  # Salta a la etiqueta especificada si el valor es igual a cero.
    elif codigo_op == comando_jump_gt_0:
        etiqueta = programa[ip]
        ip += 1
        salida.write("; -- JUMP.GT.0 ---\n")
        salida.write("\tCMP qword [rsp], 0\n")  # Compara el valor en la cima de la pila con cero.
        salida.write(f"\tJG {etiqueta}\n")  # Salta a la etiqueta especificada si el valor es mayor que cero.
    elif codigo_op == comando_halt:  # Esta insutrcción detiene el programa.
        salida.write("; -- HALT ---\n")
        salida.write("\tJMP EXIT_LABEL\n")

# código para la salida del programa
salida.write("EXIT_LABEL:\n")
salida.write("\tXOR rax, rax\n")
salida.write("\tCALL exit\n")

salida.close()


# Ojo con esto porque hay que tener "gcc" y "nams" instalado en el sistema,
# otra alternativa es ensamblar en un proveedor web que lea .asm
print("[CMD] Assembling")
comando = f"nasm -felf64 {asm_path} -o {asm_path[:-4] + '.o'}"
print(comando, "\n") if verbose else None
os.system(comando)

print("[CMD] Linking")
comando = f"gcc -o {path_programa.rsplit('.', 1)[0]+ '.exe'} {asm_path[:-3] + 'o'}"
print(comando, "\n") if verbose else None
os.system(comando)

print("[CMD] Cache deletion")
print(f"rm {asm_path[:-3] + 'o'}", "\n") if verbose else None
os.remove(asm_path[:-3] + 'o')


print("[CMD] Running")
comando = f"{path_programa.rsplit('.', 1)[0] + '.exe'}"
print(comando, "\n") if verbose else None
os.system(comando)
