## Linux Commands


> What are some useful commands to create with Linux aliases ? 

Linux aliases are a way to create custom commands or shortcuts for commonly used commands. Here are some examples of useful aliases that can be created in Linux:

- alias ll='ls -alF': This alias will list all files in the current directory, along with their permissions, in a long format.

- alias gs='git status': This alias will show the status of the Git repository.

- alias ..='cd ..': This alias will take you one level up in the directory structure.

- alias lsa='ls -lah': This alias will list all files (including hidden ones) in a long format.

- alias cp='cp -i': This alias will prompt for confirmation before overwriting an existing file while copying.

- alias c='clear': This alias will clear the terminal screen.

- alias grep='grep --color=auto': This alias will highlight the matched text in the output of the grep command.

- alias hist='history | grep': This alias will search your command history for a specific command.

- alias rd='rmdir': This alias will remove a directory without any prompt or confirmation.

- alias update='sudo apt-get update && sudo apt-get upgrade': This alias will update the Linux system packages to their latest versions.

---

> Can you provide an argument to an alias

Yes, you can provide an argument to an alias by using the $1, $2, $3, etc. placeholders in the alias command. For example, let's say you want to create an alias that opens a specific directory in the terminal. You can define the alias as follows:

```bash
alias opendirectory='cd $1'
```

Here, the $1 placeholder will be replaced by the first argument that you pass to the opendirectory alias. So, if you want to open the ~/Documents directory, you can run the following command:

```
opendirectory ~/Documents
```

This will execute the cd ~/Documents command, which will change your current directory to ~/Documents.

---
> Can you place the alias's in a startup file so that you don't have to recreate them? 

Yes, you can place the aliases in a startup file so that you don't have to recreate them every time you open a new terminal window. In Linux, the startup file is called .bashrc, and it's located in your home directory. You can edit this file to add your aliases.

To add an alias to .bashrc, you can open the file using a text editor such as nano:

```bash
nano ~/.bashrc
```

Then add your alias to the end of the file:

```bash
alias l='ls -alh'
```

Save the file and exit the editor. Now, the alias will be loaded automatically every time you open a new terminal window. You can also source the .bashrc file manually by running the command source ~/.bashrc.

---
> Which directory is .bashrc in ? 

The .bashrc file is typically located in the user's home directory. The full path to the file is ~/.bashrc, where ~ is a shorthand for the user's home directory.

---
> How do i get my user name? then is that related to the name of my home directory ? 

You can get your user name by typing the command whoami in the terminal. The name of your home directory is usually the same as your user name, but it can be different if you have changed it manually. You can check your home directory by typing echo $HOME in the terminal.

---
> Where do you put general aliases in the .bashrc  file? 

You can put general aliases in the .bashrc file anywhere between the top and bottom of the file. It's a matter of personal preference and organization. However, a common convention is to put them near the top of the file so they are easy to find and modify. Some people also group related aliases together by creating sections and adding comments to describe them.

---
> If $1 is an argument in the alias, can you add text to end, like $1.cob , ? 

Yes, you can append text to the end of an argument in an alias by using the $1 syntax and then adding the desired text after it. For example, if you want to pass an argument to an alias and append ".cob" to the end of it, you can define the alias in your .bashrc file as follows:

```bash
alias myalias='cobc -x $1.cob'
```

In this example, the $1 variable represents the first argument passed to the alias, and the ".cob" text is appended to the end of it using the . syntax. So, if you run the command myalias myfile, it will be expanded to cobc -x myfile.cob.

---
> How can I get an alias to compile and run a COBOL program? 

You could use the following syntax to pass an argument to the alias:

```rust
alias cobol='function _cobol() { cobc -g -x "$1".cob; ./"$1"; }; _cobol'
```

This defines a shell function called _cobol() inside the alias, which takes the first argument passed to cobol and uses it to compile and run a COBOL program. The function is then immediately called with the "$1" parameter to execute the alias.

---
### REFERENCES

Shotts, W. (2019). The Linux command line: a complete introduction. No Starch Press.




