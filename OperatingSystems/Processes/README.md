## Processes 

**Table of Contents:**
- POSIX Process Management API 
- Memory Protection
- Access Control: DAC (Discretionary Access Control) and MAC (Mandatory Access Control)
- Linux Process Commands 

---
> Please describe POSIX Process Management API

The POSIX (Portable Operating System Interface) Process Management API provides a set of functions and interfaces that allow programs to manage processes and perform inter-process communication (IPC) on Unix and Unix-like systems. The API defines a standardized interface that ensures compatibility across different implementations of the Unix operating system.

Some of the key functions in the POSIX Process Management API include: 

1. fork(): This function creates a new process by duplicating the calling process. The new process is referred to as the child process, while the original process is referred to as the parent process. 
2. exec(): This function replaces the current process image with a new process image. It is commonly used to run a new program in the current process. 
3. wait(): This function suspends the calling process until one of its child processes terminates. It also returns information about the terminated process. 
4. kill(): This function sends a signal to a process or a group of processes. Signals are used to interrupt or communicate with a process. 
5. pipe(): This function creates a unidirectional communication channel between two processes. It is commonly used for IPC. 
6. signal(): This function sets a signal handler function for a specific signal. The handler function is called when the signal is received by the process.

Overall, the POSIX Process Management API provides a powerful set of tools for managing and communicating between processes on Unix systems.

---
> Please describe how memory protection works with processes.

Memory protection is a crucial mechanism that helps ensure the stability and security of modern computer systems. In the context of processes, memory protection refers to the ability of the operating system to isolate and protect the memory used by each individual process.

When a process is created, the operating system assigns a virtual address space to the process. This address space is a set of memory addresses that the process can use to store data and instructions. However, the actual physical memory used by the process may be scattered across different locations in the physical memory of the system.

To protect the memory used by each process, the operating system sets up a memory management unit (MMU) that maps virtual addresses used by the process to the physical memory locations. The MMU uses a page table that associates virtual memory pages with physical memory pages.

In addition, the MMU enforces memory protection by setting permissions on each memory page. Each page can be marked as readable, writable, or executable. The operating system can also mark pages as inaccessible to prevent processes from accessing certain areas of memory.

This memory protection mechanism helps prevent processes from accidentally or maliciously accessing or modifying memory that belongs to another process. If a process tries to access a memory location that is not permitted by the page table, the MMU generates a hardware exception that is handled by the operating system. This can result in the process being terminated or in an error message being displayed.

Overall, memory protection is a critical aspect of process management, as it ensures that processes can operate independently without interfering with each other's memory.

---
> Please Differentiate between a DAC system and a MAC system in operating systems

DAC (Discretionary Access Control) and MAC (Mandatory Access Control) are two different access control models used in operating systems to regulate access to system resources. The main difference between the two is in how they enforce access control.

In a **DAC system**, access control is at the discretion of the owner of the resource. Each resource has an owner who decides who can access the resource and what level of access they have. This means that the owner of a resource has complete control over who can access it and can grant or revoke access permissions as they see fit. However, this can also lead to security issues, as an untrusted user who gains access to a resource can potentially modify or delete it, even if they do not have permission to do so.

In contrast, in a **MAC system**, access control is enforced by a centralized authority, such as the operating system or a security administrator. Access permissions are assigned based on a set of predefined rules and policies, and users have no control over these permissions. This means that even if a user gains access to a resource, they may not be able to modify or delete it if they do not have the necessary permissions according to the policy.

One advantage of a MAC system is that it provides a more secure environment, as access to resources is strictly controlled by a centralized authority, rather than being left to the discretion of individual users. However, this can also make it more difficult for users to access resources that they need to perform their tasks, as they must request permission from the authority.

Overall, DAC and MAC are two different approaches to access control, each with its own advantages and disadvantages. The choice between the two depends on the specific security needs and requirements of the system.

---
> Please enumerate and describe the process commands for the Linux command line.

The Linux command line provides a wide range of commands for managing processes. Some of the commonly used process commands are: 

1. ps - This command displays a list of currently running processes, including their process IDs (PIDs), CPU and memory usage, and other details. 
2. top - This command provides a real-time view of system processes, including the most CPU-intensive processes, memory usage, and other statistics. 
3. kill - This command sends a signal to a specified process to terminate it. The default signal is SIGTERM, which requests a graceful termination of the process, but other signals such as SIGKILL can be used to force a process to terminate immediately. 
4. nice - This command is used to set the priority of a process. By default, processes have a priority of 0, but the nice command can be used to adjust the priority level from -20 (highest priority) to 19 (lowest priority). 
5. renice - This command is used to change the priority of a running process. It is similar to the nice command but can be used to adjust the priority of a process that is already running. 
6. pidof - This command returns the PID of a process based on its name. This is useful when you need to find the PID of a specific process to use with other commands. 
7. top - This command provides a real-time view of system processes, including the most CPU-intensive processes, memory usage, and other statistics. 
8. pstree - This command displays a tree-like view of processes, showing their parent-child relationships. 
9. pgrep - This command returns the PID of a process based on its name and other criteria, such as user ID or parent process ID. 
10. fg - This command brings a background process to the foreground.

Overall, these commands provide a powerful set of tools for managing processes on Linux systems, enabling users to monitor, control, and manipulate system processes to meet their specific needs.

---
REFERENCES 

- Wikipedia: [POSIX](https://en.wikipedia.org/wiki/POSIX), [Process](https://en.wikipedia.org/wiki/Process_(computing)),
[Mandatory access control](https://en.wikipedia.org/wiki/Mandatory_access_control), [Discretionary access control](https://en.wikipedia.org/wiki/Discretionary_access_control), [Process group](https://en.wikipedia.org/wiki/Process_group), 
[Fork](https://en.wikipedia.org/wiki/Fork_(system_call)), [System call](https://en.wikipedia.org/wiki/System_call), [Zombie process](https://en.wikipedia.org/wiki/Zombie_process), [Kernel](https://en.wikipedia.org/wiki/Kernel_(operating_system))

