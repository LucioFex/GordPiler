"""
Compilador: GordPiler
---------------------
Agustín Brogliatti
Luciano Esteban
"""
import sys


path_programa = sys.argv[1]

# Lectura de líneas de archivo
lineas_programa = []
with open(path_programa, "r") as archivo_programa:
    lineas_programa = [linea.strip() for linea in archivo_programa.readlines()]


programa = []

# Analizador léxico:

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
    elif codigo_op == "JUMP.GT.0":
        # Lee etiqueta
        etiqueta = partes[1]
        programa.append(etiqueta)


# Log para testing
print(programa)
