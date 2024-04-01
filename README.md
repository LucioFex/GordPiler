# GordPiler

GordPiler es un compilador simple desarrollado en python que traduce un lenguaje de programación personalizado (denominado ".gord") a ensamblador x86_64.

## Características

- Capacidad para leer programas escritos en el lenguaje personalizado (.gord).
- Traducción de código .gord a ensamblador x86_64.
- El leunguaje .gord tiene la capacidad de abstraer las siguientes instrucciones de assembly: PUSH, POP, ADD, SUB, JUMP, PRINT y READ.
- Uso de etiquetas para el control de flujo del programa.
- Gestión de literales de cadena (strings) para la instrucción PRINT.

## Requisitos

- Python 3.x
- NASM (Netwide Assembler)
- GCC (GNU Compiler Collection)

## Uso

Para compilar un programa escrito en el lenguaje .gord, sigue estos pasos:

1. Asegúrate de tener instalados los requisitos mencionados anteriormente.
2. Ejecuta el compilador de la siguiente manera:

    ```bash
    python gordpiler.py tu_programa.gord
    ```

3. El compilador generará un archivo ensamblador (.asm) basado en tu programa .gord.
4. Luego, el compilador ensambla y enlaza el archivo .asm generado para crear un ejecutable.
5. Finalmente, el ejecutable resultante se ejecuta y se muestra la salida del programa.

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