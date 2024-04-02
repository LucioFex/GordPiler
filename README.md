# GordPiler

<h3>
    GordPiler es un compilador simple, desarrollado en Python.<br>
    Traduce el lenguaje de programación personalizado (denominado ".gord") a ensamblador (con la convención utilizada para sistemas operativos Windows 10 de 64 bits, INTEL/AMD - x86_64).
</h3>

## Características

- Capacidad para leer programas escritos en el lenguaje personalizado (.gord).
- Traducción de código .gord a ensamblador x86_64.
- El leunguaje .gord tiene la capacidad de abstraer las siguientes instrucciones de assembly: PUSH, POP, ADD, SUB, JUMP, PRINT y READ.
- Uso de etiquetas para el control de flujo del programa.
- Gestión de literales de cadena (strings) para la instrucción PRINT.

## Requisitos

- Python 3.11.x.
- NASM 2.16 (Netwide Assembler).
- GCC 12.2.x (GNU Compiler Collection).
- OS Windows 10.

## Uso

Para compilar un programa escrito en el lenguaje .gord, sigue estos pasos:

1. Asegúrate de tener instalados los requisitos mencionados anteriormente.
2. Ejecuta el compilador de la siguiente manera:

    ```bash
    python compilador.py programa_1.gord verbose
    ```

3. El compilador generará un archivo ensamblador (.asm) basado en tu programa .gord.
4. Luego, el compilador ensambla y genera un archivo binario, pasando de uno .asm a .o.
5. Posterior a esto, se toma este archivo binario y se enlaza a uno ejecutable mediante GCC.
6. Finalmente, el .exe resultante se ejecuta y se muestra la salida del programa escrito en el lenguaje .gord.

## Ejemplo

Supongamos que tenemos el siguiente programa en .gord:

```plaintext
READ
READ
SUB
JUMP.EQ.0 L1
PRINT "not equal"
HALT

L1:
PRINT "equal"
HALT
```

## Autores

- Agustín Brogliatti
- Luciano Esteban

## Licencia

Este proyecto está bajo la [Licencia MIT](LICENSE).
