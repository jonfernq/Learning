## 2. CPU Scheduling 

> For operating systems, what are the different types of Central Processing Unit (CPU) Scheduling? What are the scheduling criteria and what algorithms are used? 

In operating systems, the CPU scheduling algorithm is responsible for selecting which process should be executed next on the CPU. There are several different types of CPU scheduling algorithms, each with its own criteria and algorithms. Here are some of the most common ones:

- **First-Come, First-Served (FCFS):** This scheduling algorithm selects the process that arrived first and executes it until completion. The criteria used for this algorithm is the arrival time of the process. The algorithm used is a simple FIFO (First-In, First-Out) queue.

- **Shortest Job First (SJF):** This algorithm selects the process with the shortest estimated running time and executes it until completion. The criteria used for this algorithm is the burst time of the process. The algorithm used is non-preemptive SJF or preemptive SJF.

- **Priority Scheduling:** In this algorithm, each process is assigned a priority, and the process with the highest priority is executed first. The criteria used for this algorithm is the priority level of the process. The algorithm used is non-preemptive priority scheduling or preemptive priority scheduling.

- **Round Robin (RR):** This algorithm assigns a fixed time slice to each process, and each process is executed for that amount of time before being preempted and placed back in the ready queue. The criteria used for this algorithm is the time slice assigned to each process. The algorithm used is preemptive round-robin scheduling.

- **Multi-level Queue Scheduling:** In this algorithm, the ready queue is divided into multiple levels, and each level has its own scheduling algorithm. The criteria used for this algorithm is the priority level of the process. The algorithm used is non-preemptive or preemptive scheduling.

- **Multi-level Feedback Queue Scheduling:** This algorithm is similar to multi-level queue scheduling, but it allows a process to move between queues based on its behavior. The criteria used for this algorithm is the behavior of the process. The algorithm used is preemptive scheduling.

These are some of the most common CPU scheduling algorithms used in operating systems. The choice of scheduling algorithm depends on the specific requirements of the system and the trade-offs between response time, throughput, and fairness.

