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

### First-Come, First-Served (FCFS): Python Code

Please describe First-Come, First-Served (FCFS) with a simple Python program. 
First-Come, First-Served (FCFS) scheduling algorithm is a non-preemptive algorithm that selects the process that arrived first and executes it until completion. Here is a simple Python program to implement the FCFS scheduling algorithm:

Here the code displays the time sequence of processes executing and also the processing and waiting time for each process:

```python
class Process:
    def __init__(self, pid, arrival_time, burst_time):
        self.pid = pid
        self.arrival_time = arrival_time
        self.burst_time = burst_time
        self.completion_time = 0
        self.waiting_time = 0

def fcfs(processes):
    time_sequence = []
    completion_time = 0
    waiting_time = 0

    for process in processes:
        completion_time += process.burst_time
        process.completion_time = completion_time
        process.waiting_time = completion_time - process.arrival_time - process.burst_time
        waiting_time += process.waiting_time
        time_sequence.append(process.pid)

    average_waiting_time = waiting_time / len(processes)
    return average_waiting_time, time_sequence

# Sample Input

processes = [Process(1, 0, 6), Process(2, 2, 4), Process(3, 4, 2), Process(4, 5, 5)]

# Run the algorithm and print the result

avg_waiting_time, time_sequence = fcfs(processes)
print("Average Waiting Time:", avg_waiting_time)
print("Time Sequence:", time_sequence)

for process in processes:
    print(f"Process {process.pid}: Processing Time = {process.burst_time}, Waiting Time = {process.waiting_time}")
```

The Python code above demonstrates the implementation of the First-Come, First-Served (FCFS) scheduling algorithm for a list of processes.

The Process class is defined to store the process ID, arrival time, burst time, completion time, and waiting time of each process.

The fcfs function takes a list of processes and calculates the average waiting time using the FCFS algorithm. It also keeps track of the time sequence of processes executing and the processing and waiting time for each process.

In the fcfs function, the completion time and waiting time are calculated for each process, and the process's ID is added to the time_sequence list in the order in which they are executed.

Finally, the fcfs function returns the average waiting time and the time_sequence list. The program then prints the average waiting time, time sequence, and processing and waiting time for each process.

With the modifications made, this program provides a comprehensive display of the FCFS algorithm execution, including the time sequence of processes executing and the processing and waiting time for each process.


