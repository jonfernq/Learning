## WSL Ubuntu

### Hello World

> Please give me a simple C program to compile in WSL Ubuntu, along with instructions on how to compile.

Here's a simple C program that you can compile in WSL Ubuntu:

```arduino
#include <stdio.h>

int main() {
    printf("Hello, World!\n");
    return 0;
}
```

Open Ubuntu, then:

```
root@DESKTOP-GBKF6GA:~# cd /mnt/d
root@DESKTOP-GBKF6GA:/mnt/d# mkdir myprogram
root@DESKTOP-GBKF6GA:/mnt/d# cd myprogram
```

To compile this program in WSL Ubuntu, you can follow these steps:

Open a terminal in WSL Ubuntu.

Navigate to the directory where your C program is saved using the cd command. For example, if your program is saved in a directory called my_program, you can navigate to it with the following command:

```bash
cd my_program
```

Compile the program using the gcc command. For example, if your program is saved in a file called hello.c, you can compile it with the following command:

``` 
gcc -o hello hello.c
```

This command tells the gcc compiler to compile the hello.c file and create an executable file called hello in the same directory. The -o flag specifies the name of the output file.

Run the program by typing its name in the terminal and pressing Enter. For example, to run the hello program, type the following command:

```bash
./hello
```

This will execute the program and print "Hello, World!" to the terminal.

That's it! You should now have a working C program that you compiled in WSL Ubuntu.

---
> Please give me a hello world cobol program for WSL Ubuntu to compile and run. 

Here's a "Hello, World!" program in COBOL that you can compile and run in WSL Ubuntu:

```sql
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. HELLO-WORLD.
000300 PROCEDURE DIVISION.
000400     DISPLAY "Hello World!".
000500     STOP RUN.
```

To compile and run this program, you'll need to have a COBOL compiler installed in your WSL Ubuntu environment. One popular COBOL compiler for Linux is open-cobol, which you can install using the following command in your terminal:

```arduino
sudo apt-get install open-cobol
```

Once you have open-cobol installed, you can compile the "Hello, World!" program by saving it in a file called hello.cbl and running the following command in your terminal:

```
cobc -x hello.cbl
```

This will compile the program and create an executable file called hello in the same directory. The -x option tells cobc to create an executable file.

Finally, you can run the program by typing the following command in your terminal:

```bash
./hello
```

This will execute the program and print "Hello, World!" to the terminal.

That's it! You should now have a working "Hello, World!" program in COBOL that you compiled and ran in WSL Ubuntu.


