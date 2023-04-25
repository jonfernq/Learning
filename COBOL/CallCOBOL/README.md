### CALL (COBOL) & GNUCobol: Static Linking with C Programs


**Bash: hellocall.sh**

```
#!/bin/bash

# hellocall.sh - compile C hello World program 
# linked in calling COBOL program

# Change permissions on file: chmod +x hellocall.sh
# Change end of line of file: Edit > EOL COnversion > Linux (LF)  
# called like this: ./hellocall.sh 

# Compile C program
cc -c say.c

# Compile COBOL program
cobc -x -static -free hellocall.cob say.o

# Execute COBOL program
./hellocall
```

---
**C Program: say.c**

```
#include <stdio.h>

int say(char *hello, char *world)
{
  int i;
  for (i = 0; i < 6; i++)
    putchar(hello[i]);
  for (i = 0; i < 6; i++)
    putchar(world[i]);
  putchar('\n');
  return 0;
}
```

---
**COBOL: hellocall.cob** 

```
IDENTIFICATION DIVISION.
PROGRAM-ID. hello.
ENVIRONMENT DIVISION.
DATA DIVISION.
WORKING-STORAGE SECTION.
	01 HELLO PIC X(6) VALUE "Hello ".
	01 WORLD PIC X(6) VALUE "World!".
PROCEDURE DIVISION.
	CALL "say" USING HELLO WORLD.
	STOP RUN.
```

**Source:** [GNUCobol: Static Linking with C Programs](https://gnucobol.sourceforge.io/historical/open-cobol/Static-COBOL-to-C.html) 


