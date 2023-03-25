## WSL Ubuntu

### Hello World

Please give me a simple C program to compile in WSL Ubuntu, along with instructions on how to compile.

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
