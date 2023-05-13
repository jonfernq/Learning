# Who & Whoami Commands

> I get no output for the command 'who' 
but I do get output for 'whoami' 
what is the meaning of this output? 

root@DESKTOP-GBKF6GA:/mnt/c/Users/ACER# who
root@DESKTOP-GBKF6GA:/mnt/c/Users/ACER# whoami
root

The meaning of this output is:

- The who command is used to display information about the users who are currently logged in to the system. It shows the user name, terminal name, login time, and remote host name for each user. However, if there are no users logged in to the system, or if the system does not keep track of user logins, the who command will produce no output.
- The whoami command is used to display the user name of the current effective user. It shows the user name that is associated with the current process or shell. It does not depend on whether the user is logged in or not. In this case, the whoami command shows that the current effective user is root, which is the superuser or administrator of the system.

---


