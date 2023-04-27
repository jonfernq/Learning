## Processes 

**Table of Contents:**
- POSIX Process Management API 
- Memory Protection
- Access Control: DAC (Discretionary Access Control) and MAC (Mandatory Access Control)
- Linux Process Commands 
- Minix Internals
- References

---
> Please define the idea of a 'process' that executes or runs on an operating system.

In an operating system, a process is an instance of a program that is being executed. It is a unit of work that the operating system can schedule for execution and manage resources for. A process consists of an executable program code, memory space, system resources, and one or more threads of execution.

Each process is assigned a unique identifier known as the process identifier (PID) that can be used to identify, monitor, and manage the process. The operating system uses the process control block (PCB) to store the process state, context, and other related information such as the process priority, scheduling information, and memory management.

Processes are a fundamental concept in operating systems, and they allow the operating system to manage multiple tasks simultaneously by dividing them into separate processes that can run concurrently or in parallel.

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
### Minix

> In the Minix operating system what does the data structure for a process look like?

 
In the Minix operating system, a process is represented by a task structure or `struct task`. This structure contains important information about the process, including: 
1. `t_stack`: A pointer to the process stack. 
2. `t_regs`: A structure that holds the current CPU registers. 
3. `t_priority`: The process priority. 
4. `t_quantum`: The amount of CPU time the process is allowed to run before being preempted. 
5. `t_memmap`: A pointer to the process's memory map. 
6. `t_endpoint`: A unique endpoint number that identifies the process. 
7. `t_flags`: Various flags that indicate the state of the process.

There are other fields in the `struct task` structure as well, but these are some of the most important.

--- 
> What is the basic algorithm used for executing processes on Minix?

 
The basic algorithm used for executing processes on Minix is known as the round-robin scheduling algorithm. This algorithm works by assigning a time slice or quantum to each process in the system, allowing it to run for a specific amount of time before being preempted and replaced by another process. This time slice is typically measured in milliseconds and is adjustable based on system requirements. When a process is preempted, its current state is saved so that it can resume execution later, and the scheduler selects the next process to run based on a priority system or other criteria. This algorithm ensures that each process in the system has a fair share of CPU time and prevents any one process from monopolizing system resources.

--- 
> What is the process stack ?

 
The process stack is a region of memory used by a process to store local variables, function call parameters, and other information related to function calls. It is a Last-In-First-Out (LIFO) data structure where each function call pushes a new frame onto the stack, and each return pops the frame off the stack. The stack is managed by the operating system as part of the process's memory space and grows and shrinks dynamically as the process calls and returns from functions. The process stack is an essential component of the call stack, which is a general term for the stack of function calls and related data used by a program during execution.

--- 
> What are the possible states of a Minix process ?

 
In Minix, a process can be in one of the following states:
1. RUNNING: The process is currently running on a processor.
2. READY: The process is ready to run, but is waiting for a processor.
3. BLOCKED: The process is waiting for some event to occur before it can continue executing. This could be waiting for I/O to complete or for a message from another process.
4. SEND: The process has sent a message to another process and is waiting for a reply.
5. RECEIVE: The process is waiting to receive a message from another process.
6. INTERRUPTED: The process has been interrupted by some event, such as a timer interrupt.

These states can change as a process is executed and interacts with other processes and the operating system.

--- 
> How is a  process's memory map used in Minix?

 
In Minix, a process's memory map is a data structure that describes how a process's virtual memory is mapped to physical memory. It consists of a list of segments, where each segment describes a contiguous block of virtual memory in the process's address space and the corresponding physical memory or swap space where it is located.

The memory map is used by the operating system to manage the process's memory and to facilitate memory protection and sharing. When a process accesses a memory location, the operating system uses the memory map to translate the virtual address to a physical address and to determine whether the access is valid or not.

The memory map also provides information about the permissions and attributes of each segment, such as read, write, and execute permissions, and whether the segment is shared or private to the process. This information is used by the operating system to enforce memory protection and to manage memory resources efficiently.

--- 
> What do process stack data structures look like ?

 
In most operating systems, including Minix, the process stack is implemented as a stack data structure, which is a simple data structure that follows the "last in, first out" (LIFO) principle. It is implemented using a contiguous block of memory that is divided into fixed-size frames or pages. The stack pointer (SP) keeps track of the top of the stack, which is the location of the last pushed item. The stack grows in the opposite direction of the heap, typically from high memory addresses to low memory addresses.

Each stack frame contains the function arguments, local variables, and return address of a function call. When a function is called, a new stack frame is pushed onto the stack, and when a function returns, its stack frame is popped from the stack, and the return address is used to continue execution at the point where the function was called.

The stack frame typically contains the following elements:
- Function arguments: the values passed to the function when it was called
- Local variables: the variables declared within the function's body
- Return address: the memory address where execution should continue after the function call returns
- Frame pointer: a pointer to the base of the stack frame, used to access the function's arguments and local variables

The exact layout of the stack frame depends on the calling convention used by the operating system and the CPU architecture.

--- 
> How does one locate the heap for a process?

 
In general, the heap of a process is dynamically allocated memory that is used to store data that is created during the execution of the program. In most operating systems, including Minix, the heap is located in the higher end of the process's virtual address space, typically above the text (code) and data segments.

In Minix, the heap is created dynamically as the process needs it. When the process requests memory allocation using functions like malloc() or new(), the operating system allocates memory from the heap area of the process's address space.

The heap data structure itself is managed by the operating system's memory management system, which keeps track of which memory blocks are in use and which are free. This information is used to allocate and free memory blocks on demand.

---
### REFERENCES

Anderson, T., & Dahlin, M. (2011-15). *Operating Systems: Principles and Practice, Vol. 1: Kernels and Processes.* Recursive Books.

Arpaci-Dusseau, R. H., & Arpaci-Dusseau, A. C. (2014). *Operating Systems: Three Easy Pieces.* Arpaci-Dusseau Books.

Bryant, R. E., & O'Hallaron, D. R. (2015). *Computer Systems: A Programmer's Perspective.* Pearson.

Hailperin, M. (2019). *Operating systems and middleware: Supporting controlled interaction.* Thomson Learning, Inc.: San Francisco, CA.

Silberschatz, A., Galvin, P. B., & Gagne, G. (2018). *Operating System Concepts.* Wiley.

Stallings, W. (2018). *Operating Systems: Internals and Design Principles.* Pearson.

Tanenbaum, A. S. (1987). *Operating Systems: Design and Implementation.* Pearson Education.

Tanenbaum, A. S., & Bos, H. (2014). *Modern Operating Systems.* Pearson.

Russinovich, M., Solomon, D. A., & Ionescu, A. (2017). *Windows Internals.* Microsoft Press.

- Wikipedia: [POSIX](https://en.wikipedia.org/wiki/POSIX), [Process](https://en.wikipedia.org/wiki/Process_(computing)),
[Mandatory access control](https://en.wikipedia.org/wiki/Mandatory_access_control), [Discretionary access control](https://en.wikipedia.org/wiki/Discretionary_access_control), [Process group](https://en.wikipedia.org/wiki/Process_group), 
[Fork](https://en.wikipedia.org/wiki/Fork_(system_call)), [System call](https://en.wikipedia.org/wiki/System_call), [Zombie process](https://en.wikipedia.org/wiki/Zombie_process), [Kernel](https://en.wikipedia.org/wiki/Kernel_(operating_system))

