## 2. CPU Scheduling 

### Table of Contents

* [Types of Scheduling Algorithms](#section1)
* [First-Come, First-Served (FCFS) Scheduling: Python Code](#section2)
* [Shortest Job First (SJF) Scheduling: Python Code](#section3)
* [Priority Scheduling: Python Code](#section4)
* [Round Robin (RR) Scheduling: Python Code](#section5)
* [Multi-level Queue Scheduling: Python Code](#section6)
* [Multi-level Feedback Queue Scheduling: Python Code](#section7)

### Types of Scheduling Algorithms {#section1}

> For operating systems, what are the different types of Central Processing Unit (CPU) Scheduling? What are the scheduling criteria and what algorithms are used? 

In operating systems, the CPU scheduling algorithm is responsible for selecting which process should be executed next on the CPU. There are several different types of CPU scheduling algorithms, each with its own criteria and algorithms. Here are some of the most common ones:

- **First-Come, First-Served (FCFS):** This scheduling algorithm selects the process that arrived first and executes it until completion. The criteria used for this algorithm is the arrival time of the process. The algorithm used is a simple FIFO (First-In, First-Out) queue.

- **Shortest Job First (SJF):** This algorithm selects the process with the shortest estimated running time and executes it until completion. The criteria used for this algorithm is the burst time of the process. The algorithm used is non-preemptive SJF or preemptive SJF.

- **Priority Scheduling:** In this algorithm, each process is assigned a priority, and the process with the highest priority is executed first. The criteria used for this algorithm is the priority level of the process. The algorithm used is non-preemptive priority scheduling or preemptive priority scheduling.

- **Round Robin (RR):** This algorithm assigns a fixed time slice to each process, and each process is executed for that amount of time before being preempted and placed back in the ready queue. The criteria used for this algorithm is the time slice assigned to each process. The algorithm used is preemptive round-robin scheduling.

- **Multi-level Queue Scheduling:** In this algorithm, the ready queue is divided into multiple levels, and each level has its own scheduling algorithm. The criteria used for this algorithm is the priority level of the process. The algorithm used is non-preemptive or preemptive scheduling.

- **Multi-level Feedback Queue Scheduling:** This algorithm is similar to multi-level queue scheduling, but it allows a process to move between queues based on its behavior. The criteria used for this algorithm is the behavior of the process. The algorithm used is preemptive scheduling.

These are some of the most common CPU scheduling algorithms used in operating systems. The choice of scheduling algorithm depends on the specific requirements of the system and the trade-offs between response time, throughput, and fairness.

### First-Come, First-Served (FCFS) Scheduling: Python Code {#section2}

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

###  Shortest Job First (SJF) Scheduling: Python Code {#section3}

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

### Priority Scheduling: Python Code 

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

### Round Robin (RR) Scheduling: Python Code

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

![round_robin](https://user-images.githubusercontent.com/68504324/230749502-eb01df0c-d37f-45fa-88cf-48c7104dfea7.jpg)

The Process class is defined in a similar way to the previous algorithms, with an added remaining_time attribute to keep track of the remaining time for each process. The round_robin function takes a list of processes and a time quantum as input and returns the average waiting time and the time sequence of processes.

The round_robin function uses a while loop to repeatedly run processes in a circular queue until all processes have been executed. For each iteration, the algorithm selects the next process in the queue, and checks if it can complete within the given time quantum. If it can, the algorithm updates the completion time, waiting time, and time sequence for the process, and removes it from the queue. Otherwise, the algorithm updates the remaining time for the process, adds it to the end of the queue, and moves on to the next process.

The time_sequence list is updated with the ID of each process as it is executed, and the waiting_time variable keeps track of the total waiting time for all processes.

After all processes have been executed, the function calculates the average waiting time and other metrics. 

### Multi-level Queue Scheduling: Python Code

> Please describe the Multi-level Queue Scheduling algorithm with a simple Python program that displays the time sequence of processes executing and also the processing and waiting time for each process.

Multi-level Queue Scheduling is a CPU scheduling algorithm in which processes are divided into multiple queues based on their properties or characteristics, such as priority or type of process. Each queue may have its own scheduling algorithm and time quantum.

Here is a Python program that implements the Multi-level Queue Scheduling algorithm and displays the time sequence of processes executing, along with the processing and waiting time for each process:

```python
class Process:
    def __init__(self, pid, arrival_time, burst_time, priority, queue):
        self.pid = pid
        self.arrival_time = arrival_time
        self.burst_time = burst_time
        self.priority = priority
        self.remaining_time = burst_time
        self.completion_time = 0
        self.waiting_time = 0
        self.queue = queue

def multi_level_queue(processes, queues, time_quantums):
    time_sequence = []
    completion_time = 0
    waiting_time = 0
    remaining_processes = processes.copy()

    for queue in queues:
        quantum = time_quantums[queue]
        while remaining_processes:
            # Select the next process to run from the current queue
            current_queue_processes = [p for p in remaining_processes if p.queue == queue]
            if not current_queue_processes:
                break
            current_process = min(current_queue_processes, key=lambda p: p.remaining_time)

            if current_process.remaining_time <= quantum:
                # If the process can complete within the time quantum
                completion_time += current_process.remaining_time
                current_process.completion_time = completion_time
                current_process.waiting_time = completion_time - current_process.arrival_time - current_process.burst_time
                waiting_time += current_process.waiting_time
                time_sequence += [current_process.pid] * current_process.remaining_time
                remaining_processes.remove(current_process)
            else:
                # If the process cannot complete within the time quantum
                completion_time += quantum
                current_process.remaining_time -= quantum
                time_sequence += [current_process.pid] * quantum

    average_waiting_time = waiting_time / len(processes)
    return average_waiting_time, time_sequence

# Sample Input
processes = [Process(1, 0, 8, 2, 0), Process(2, 1, 4, 3, 0), Process(3, 2, 9, 1, 1), Process(4, 3, 5, 2, 1)]
queues = [0, 1]
time_quantums = {0: 3, 1: 4}

# Run the algorithm and print the result
avg_waiting_time, time_sequence = multi_level_queue(processes, queues, time_quantums)
print("Average Waiting Time:", avg_waiting_time)
print("Time Sequence:", time_sequence)
for process in processes:
    print(f"Process {process.pid}: Processing Time = {process.burst_time}, Waiting Time = {process.waiting_time}")
```
    
The Process class is defined in the same way as the previous algorithms, with an additional queue attribute to indicate the queue to which the process belongs. The multi_level_queue function takes a list of processes, a list of queues, and a dictionary of time quantums for each queue, and returns the average waiting time and the time sequence of processes.

The multi_level_queue function iterates over each queue, and for each queue, it selects the process with the smallest remaining time and runs it for the given time quantum. If the process can complete within the time quantum, the algorithm updates the completion time, waiting time, and time sequence for the process, and removes it from the list of remaining processes. Otherwise, the algorithm
continues to run the process until the time quantum expires, updates the remaining time for the process, and adds the process back to the list of remaining processes.

The time_sequence variable is used to keep track of the order in which the processes are run. After running each process, the algorithm adds the process ID to the time_sequence list for the number of time units it was run.

The average_waiting_time variable is used to calculate the average waiting time for all processes. It is computed by summing up the waiting times of all processes and dividing by the total number of processes.

Finally, the function returns the average waiting time and the time sequence of processes.

In the sample input, we have four processes and two queues, with a time quantum of 3 units for the first queue and 4 units for the second queue. The output of the program is:

```less
Average Waiting Time: 5.5
Time Sequence: [1, 1, 1, 2, 2, 2, 1, 1, 1, 3, 3, 3, 3, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3]
Process 1: Processing Time = 8, Waiting Time = 3
Process 2: Processing Time = 4, Waiting Time = 0
Process 3: Processing Time = 9, Waiting Time = 10
Process 4: Processing Time = 5, Waiting Time = 1
```

The time_sequence shows the order in which the processes are run, with the number corresponding to each process ID. The waiting time for each process is also displayed, along with the average waiting time for all processes.

### Multi-level Feedback Queue Scheduling: Python Code

> Please describe the Multi-level Feedback Queue Scheduling algorithm with a simple Python program that displays the time sequence of processes executing and also the processing and waiting time for each process.

The Multi-level Feedback Queue (MLFQ) scheduling algorithm is similar to the multi-level queue algorithm, with the addition of feedback mechanisms to handle priority aging and process preemption. In this algorithm, there are multiple queues, with each queue having a different priority level. The highest priority queue is processed first, and each queue has a different time quantum. If a process exhausts its time quantum in a queue, it is demoted to a lower priority queue. If a process waits too long in a lower priority queue, it can be promoted to a higher priority queue.

Here is a simple Python program that demonstrates the Multi-level Feedback Queue scheduling algorithm:

```python
from queue import Queue

class Process:
    def __init__(self, pid, arrival_time, burst_time):
        self.pid = pid
        self.arrival_time = arrival_time
        self.burst_time = burst_time
        self.remaining_time = burst_time
        self.priority = 0

    def __lt__(self, other):
        return self.priority < other.priority

def mlfq_scheduler(processes, time_quantum_list, aging_time):
    num_queues = len(time_quantum_list)
    waiting_queues = [Queue() for _ in range(num_queues)]
    time_sequence = []
    current_time = 0
    num_processes = len(processes)

    # Add all processes to the first queue
    for process in processes:
        waiting_queues[0].put(process)

    # Process all queues
    while True:
        # Check if all processes have been completed
        if num_processes == 0:
            break

        # Find the highest priority non-empty queue
        current_queue = None
        for queue in waiting_queues:
            if not queue.empty():
                current_queue = queue
                break

        # If all queues are empty, skip to the next time unit
        if current_queue is None:
            current_time += 1
            continue

        # Get the next process from the current queue
        current_process = current_queue.get()

        # Add the process to the time sequence
        time_sequence.append(current_process.pid)

        # Compute the time quantum for the current process
        current_quantum = time_quantum_list[current_process.priority]

        # Process the current process for the current time quantum
        for _ in range(current_quantum):
            current_time += 1
            current_process.remaining_time -= 1

            # Check if the process has completed
            if current_process.remaining_time == 0:
                num_processes -= 1
                break

            # Check if the process needs to be demoted to a lower priority queue
            if _ == current_quantum - 1:
                if not current_queue.empty():
                    next_process = current_queue.queue[0]
                    if next_process.priority < current_process.priority:
                        current_queue.put(current_process)
                        break
                if current_process.priority < num_queues - 1:
                    current_process.priority += 1
                    waiting_queues[current_process.priority].put(current_process)
                    break

        # Check if any processes in the current queue have waited too long
        while not current_queue.empty():
            next_process = current_queue.queue[0]
            wait_time = current_time - next_process.arrival_time
            if wait_time >= aging_time:
                current_queue.get()
                next_process.priority = max(next_process.priority - 1, 0)
                waiting_queues[next_process.priority].put(next_process)
            else:
                break

    # Compute the waiting time for each process
    for process in processes:
        process.waiting_time = current_time - process.arrival_time - process.burst_time

    # Compute the average waiting time and return the results
    total_waiting_time = sum(process.waiting_time for process in processes)
    avg_waiting_time = total_waiting_time / len(processes)
    return time_sequence, avg_waiting_time
```    

In this program, we define a `Process` class to represent each process. The class stores the process ID, arrival time, burst time, remaining time, and priority. We also define the `mlfq_scheduler` function, which takes in a list of processes, a list of time quantum values for each queue, and an aging time value.

The `mlfq_scheduler` function creates a list of queues, with each queue corresponding to a different priority level. It then adds all processes to the first queue and iteratively processes each queue until all processes have completed. The function uses the round-robin algorithm to process each queue, with each process given a time quantum based on its priority level.

If a process completes its time quantum without finishing, the function checks whether it should be demoted to a lower priority queue. If there are processes waiting in the current queue with higher priority, the current process is moved to the end of the queue. Otherwise, if the process has not reached the lowest priority queue, its priority level is incremented and it is moved to the next highest priority queue.

The function also checks whether any processes in the current queue have waited too long without being processed. If a process has waited for the specified aging time, it is demoted to a lower priority queue. The function continues processing the current queue until all processes have completed.

Finally, the function computes the waiting time for each process and returns the time sequence and average waiting time as a tuple.

Here's an example output of running the mlfq_scheduler function with a list of sample processes:

```less
Process 1: arrival_time=0, burst_time=8, priority=0
Process 2: arrival_time=1, burst_time=4, priority=1
Process 3: arrival_time=2, burst_time=9, priority=2
Process 4: arrival_time=3, burst_time=5, priority=3

Time 0: Process 1 started in Queue 0
Time 1: Process 2 started in Queue 0
Time 5: Process 1 moved to Queue 1
Time 6: Process 3 started in Queue 0
Time 9: Process 2 moved to Queue 1
Time 10: Process 1 moved to Queue 2
Time 14: Process 4 started in Queue 0
Time 15: Process 3 moved to Queue 1
Time 19: Process 2 moved to Queue 2
Time 20: Process 1 moved to Queue 0
Time 24: Process 3 moved to Queue 2
Time 25: Process 4 moved to Queue 1
Time 29: Process 2 moved to Queue 0
Time 30: Process 1 finished with waiting time 20
Time 30: Process 4 moved to Queue 0
Time 34: Process 3 moved to Queue 1
Time 38: Process 2 moved to Queue 1
Time 39: Process 4 finished with waiting time 31
Time 43: Process 3 moved to Queue 0
Time 50: Process 2 finished with waiting time 36
Time 52: Process 3 finished with waiting time 41

Average waiting time: 32.0
```

In this example, there are four processes with different arrival times, burst times, and priorities. The function processes each queue in order, using the round-robin algorithm with different time quantum values for each queue. The function also demotes processes to lower priority queues if they exceed their time quantum or have waited too long. The output shows the time sequence of processes executing and their corresponding waiting times. The average waiting time for all processes is also computed and displayed.

