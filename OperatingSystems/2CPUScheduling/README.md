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

### First-Come, First-Served (FCFS) Scheduling: Python Code

> Please describe First-Come, First-Served (FCFS) with a simple Python program that displays the time sequence of processes executing and also the processing and waiting time for each process.

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

- **Output:**
 
![fcfs](https://user-images.githubusercontent.com/68504324/230748741-8c1a2a3c-40c4-4773-b941-702893e425ff.jpg)

The Python code above demonstrates the implementation of the First-Come, First-Served (FCFS) scheduling algorithm for a list of processes.

The Process class is defined to store the process ID, arrival time, burst time, completion time, and waiting time of each process.

The fcfs function takes a list of processes and calculates the average waiting time using the FCFS algorithm. It also keeps track of the time sequence of processes executing and the processing and waiting time for each process.

In the fcfs function, the completion time and waiting time are calculated for each process, and the process's ID is added to the time_sequence list in the order in which they are executed.

Finally, the fcfs function returns the average waiting time and the time_sequence list. The program then prints the average waiting time, time sequence, and processing and waiting time for each process.

With the modifications made, this program provides a comprehensive display of the FCFS algorithm execution, including the time sequence of processes executing and the processing and waiting time for each process.

###  Shortest Job First (SJF) Scheduling: Python Code 

> Please describe the Shortest Job First (SJF) scheduling algorithm with a simple Python program that displays the time sequence of processes executing and also the processing and waiting time for each process.

The Shortest Job First (SJF) scheduling algorithm selects the process with the shortest expected processing time to be executed next. This algorithm is a non-preemptive scheduling algorithm, which means that once a process starts executing, it is not interrupted until it completes.

Here's a Python program that implements the SJF algorithm and displays the time sequence of processes executing and the processing and waiting time for each process:

```python
class Process:
    def __init__(self, pid, arrival_time, burst_time):
        self.pid = pid
        self.arrival_time = arrival_time
        self.burst_time = burst_time
        self.completion_time = 0
        self.waiting_time = 0

def sjf(processes):
    time_sequence = []
    completion_time = 0
    waiting_time = 0
    remaining_processes = processes.copy()

    while remaining_processes:
        # Find the process with the shortest remaining burst time
        shortest_process = min(remaining_processes, key=lambda p: p.burst_time)

        # Update the completion time and waiting time for the process
        completion_time += shortest_process.burst_time
        shortest_process.completion_time = completion_time
        shortest_process.waiting_time = completion_time - shortest_process.arrival_time - shortest_process.burst_time
        waiting_time += shortest_process.waiting_time
        time_sequence.append(shortest_process.pid)

        # Remove the completed process from the list of remaining processes
        remaining_processes.remove(shortest_process)

    average_waiting_time = waiting_time / len(processes)
    return average_waiting_time, time_sequence

# Sample Input
processes = [Process(1, 0, 6), Process(2, 2, 4), Process(3, 4, 2), Process(4, 5, 5)]

# Run the algorithm and print the result
avg_waiting_time, time_sequence = sjf(processes)
print("Average Waiting Time:", avg_waiting_time)
print("Time Sequence:", time_sequence)
for process in processes:
    print(f"Process {process.pid}: Processing Time = {process.burst_time}, Waiting Time = {process.waiting_time}")
```    
- **Output:**

![sjf](https://user-images.githubusercontent.com/68504324/230748910-2ec4087b-bd25-448a-b28e-03d9b894beb9.jpg)
    
The Process class is defined similarly to the FCFS algorithm, and the sjf function takes a list of processes as input.

The sjf function uses a while loop to repeatedly select the process with the shortest remaining burst time until all processes have been executed. For each iteration, the algorithm finds the process with the shortest remaining burst time, updates the completion time and waiting time for that process, and removes it from the list of remaining processes.

The time_sequence list is updated with the ID of each process as it is executed, and the waiting_time variable keeps track of the total waiting time for all processes.

After all processes have been executed, the function calculates the average waiting time and returns it along with the time_sequence list. Finally, the program prints the average waiting time, time sequence, and processing and waiting time for each process.

This program demonstrates the implementation of the SJF algorithm and displays the time sequence of processes executing and the processing and waiting time for each process.

### Priority Scheduling

Please describe the 'Priority Scheduling' algorithm with a simple Python program that displays the time sequence of processes executing and also the processing and waiting time for each process.
Priority Scheduling is a scheduling algorithm in which the process with the highest priority is executed first. Processes are assigned priorities, and the priority can be determined based on factors such as the amount of CPU time needed, the importance of the process, or its deadline.

Here is a Python program that implements Priority Scheduling algorithm and displays the time sequence of processes executing, along with the processing and waiting time for each process:

```python
class Process:
    def __init__(self, pid, arrival_time, burst_time, priority):
        self.pid = pid
        self.arrival_time = arrival_time
        self.burst_time = burst_time
        self.priority = priority
        self.completion_time = 0
        self.waiting_time = 0

def priority_scheduling(processes):
    time_sequence = []
    completion_time = 0
    waiting_time = 0
    remaining_processes = processes.copy()

    while remaining_processes:
        # Find the process with the highest priority
        highest_priority_process = min(remaining_processes, key=lambda p: p.priority)

        # Update the completion time and waiting time for the process
        completion_time += highest_priority_process.burst_time
        highest_priority_process.completion_time = completion_time
        highest_priority_process.waiting_time = completion_time - highest_priority_process.arrival_time - highest_priority_process.burst_time
        waiting_time += highest_priority_process.waiting_time
        time_sequence.append(highest_priority_process.pid)

        # Remove the completed process from the list of remaining processes
        remaining_processes.remove(highest_priority_process)

    average_waiting_time = waiting_time / len(processes)
    return average_waiting_time, time_sequence

# Sample Input
processes = [Process(1, 0, 6, 2), Process(2, 2, 4, 3), Process(3, 4, 2, 1), Process(4, 5, 5, 2)]

# Run the algorithm and print the result
avg_waiting_time, time_sequence = priority_scheduling(processes)
print("Average Waiting Time:", avg_waiting_time)
print("Time Sequence:", time_sequence)
for process in processes:
    print(f"Process {process.pid}: Processing Time = {process.burst_time}, Waiting Time = {process.waiting_time}")
```

- **Output:**

![priority py](https://user-images.githubusercontent.com/68504324/230749137-486fcece-0ee2-4754-982c-e8eb98faf2af.jpg)

The Process class is defined in a similar way to the previous algorithms, with an added priority attribute. The priority_scheduling function takes a list of processes as input and returns the average waiting time and the time sequence of processes.

The priority_scheduling function uses a while loop to repeatedly select the process with the highest priority until all processes have been executed. For each iteration, the algorithm finds the process with the highest priority, updates the completion time and waiting time for that process, and removes it from the list of remaining processes.

The time_sequence list is updated with the ID of each process as it is executed, and the waiting_time variable keeps track of the total waiting time for all processes.

After all processes have been executed, the function calculates the average waiting time and returns it along with the time_sequence list. Finally, the program prints the average waiting time, time sequence, and processing and waiting time for each process.

This program demonstrates the implementation of the Priority Scheduling algorithm and displays the time sequence of processes executing, along with the processing and waiting time for each process.

### Round Robin (RR) Scheduling

Please describe the Round Robin (RR) scheduling algorithm with a simple Python program that displays the time sequence of processes executing and also the processing and waiting time for each process.
Round Robin (RR) is a CPU scheduling algorithm in which each process is given a fixed time slot called a time quantum, and the CPU switches between processes in a circular queue. If a process has not completed its execution within the given time quantum, it is preempted, and the CPU switches to the next process in the queue.

Here is a Python program that implements the Round Robin algorithm and displays the time sequence of processes executing, along with the processing and waiting time for each process:

```python
class Process:
    def __init__(self, pid, arrival_time, burst_time):
        self.pid = pid
        self.arrival_time = arrival_time
        self.burst_time = burst_time
        self.remaining_time = burst_time
        self.completion_time = 0
        self.waiting_time = 0

def round_robin(processes, time_quantum):
    time_sequence = []
    completion_time = 0
    waiting_time = 0
    remaining_processes = processes.copy()

    while remaining_processes:
        # Select the next process to run
        current_process = remaining_processes[0]
        if current_process.remaining_time <= time_quantum:
            # If the process can complete within the time quantum
            completion_time += current_process.remaining_time
            current_process.completion_time = completion_time
            current_process.waiting_time = completion_time - current_process.arrival_time - current_process.burst_time
            waiting_time += current_process.waiting_time
            time_sequence += [current_process.pid] * current_process.remaining_time
            remaining_processes.pop(0)
        else:
            # If the process cannot complete within the time quantum
            completion_time += time_quantum
            current_process.remaining_time -= time_quantum
            time_sequence += [current_process.pid] * time_quantum
            remaining_processes.pop(0)
            remaining_processes.append(current_process)

    average_waiting_time = waiting_time / len(processes)
    return average_waiting_time, time_sequence

# Sample Input
processes = [Process(1, 0, 8), Process(2, 1, 4), Process(3, 2, 9), Process(4, 3, 5)]

# Run the algorithm and print the result
time_quantum = 3
avg_waiting_time, time_sequence = round_robin(processes, time_quantum)
print("Average Waiting Time:", avg_waiting_time)
print("Time Sequence:", time_sequence)
for process in processes:
    print(f"Process {process.pid}: Processing Time = {process.burst_time}, Waiting Time = {process.waiting_time}")
```

- **Output:**

The Process class is defined in a similar way to the previous algorithms, with an added remaining_time attribute to keep track of the remaining time for each process. The round_robin function takes a list of processes and a time quantum as input and returns the average waiting time and the time sequence of processes.

The round_robin function uses a while loop to repeatedly run processes in a circular queue until all processes have been executed. For each iteration, the algorithm selects the next process in the queue, and checks if it can complete within the given time quantum. If it can, the algorithm updates the completion time, waiting time, and time sequence for the process, and removes it from the queue. Otherwise, the algorithm updates the remaining time for the process, adds it to the end of the queue, and moves on to the next process.

The time_sequence list is updated with the ID of each process as it is executed, and the waiting_time variable keeps track of the total waiting time for all processes.

After all processes have been executed, the function calculates the average waiting time and











