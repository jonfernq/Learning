## Cloud Computing & Operating System Concepts 

> How does the concept of process relate to Docker containers running on a Linux system? 

In Docker, a container is essentially an instance of a running image. An image is a read-only template that contains a set of instructions for creating a container. When a container is created from an image, it is launched as a process that is isolated from the host system and other containers running on the same host.

In Linux, a process is a running instance of a program. It has its own memory space, system resources, and state. Similarly, a Docker container is a lightweight process that runs in its own namespace, which isolates it from other processes on the system. This allows containers to run independently, with their own resources and state, even if they are running on the same host.

The process concept is fundamental to Docker containers, as each container is essentially a running process that can be started, stopped, or managed independently from other containers running on the same host. This provides a high level of flexibility and portability, as containers can be easily moved between hosts and managed using standard process management tools.
