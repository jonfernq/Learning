## Memory Management Issues

> Analyze an operating system’s memory management. The report must convey the key areas of memory management-hardware memory management, 
operating memory management, and application memory management. The content should also explain problems relating to memory management. 
The conclusion should include which operating system is the best for your computer and recommend solutions to memory management problems. 
Images and tables are encouraged but should not constitute more than one page of the report.

The conclusion of this paper will include: 1. a choice of operating system for my computer in terms of memory management, 
and towards this end, recommended 2. 'solutions to memory management problems' encountered in the past. 
Along the way, a thorough analysis will be provided of operating system memory management at the hardware, operating system, and application levels.
Since I have already dealt with memory management theory and the parts that are largely hidden from user view in my discussion write-up,
I won't address that here. 

This report requires a comparison of at least two operating systems. The choice here will consist of: 1. Windows 10, the main OS on my notebook, together with, 2. the Ubuntu installation under Windows Sub-system for Linux (WSL) running under this Windows 10. 

Regarding my current system usage patterns, one the one hand, I have used Windows 10 on notebooks and desktops to perform everyday work and tasks for decades. 
On the other hand, it is clear that Linux is the best platform for the apps I create and the open source programming languages I use to create them, such as Python, PyTorch, and Docker.  

### Windows 10 Memory Management Experiences 

Memory itself does seem to be the key system resource causing 
ocassional performance degradation on my Windows 10 notebook. 

Several years back, moving from traditional hard drives to Solid State Drives (SSDs) 
has led to huge improvements in memory management improvement. 
Several years back when SSDs came into use, 
I started purchasing notebooks with 256GB of SSD memory with 8 GB of RAM main memory.
This seemed to be a widely available combination
that provided enough capacity while still being affordable.   
I did purchase a notebook once  with an old style much larger 
but much slower hard drive, but frustration with slow speeds 
led to its quick replacement with SSD. 

Nowadays, the only real memory management problem under Windows 10 occurs 
when I open thousands of tabs in the  Chrome browser.
Other memory intensive applications contribute to the system overload as well,
but Chrome browser memory usage always stands out in the task manager 
and the Chrome browser is known-to-be memory intensive. 

![taskmanger1](https://user-images.githubusercontent.com/68504324/233879364-c9cf236d-fb90-4c2c-852c-2fe1f6ac8de6.jpg)

![taskmanager2](https://user-images.githubusercontent.com/68504324/233879377-4b63fad3-10e9-4244-9c5f-a680d18e0ed8.jpg)

Instead of rebooting everytime I use the computer, just put it to sleep in order to easily 
save my work environment, avoiding reset up time and remembering what I was doing. 

If one does this for many days, eventually there are obvious signs that memory is tied up 
and cannot be used, some apps will not open, one cannot print from the browser, and the wi-fi tab even disappears. 
Then it is definitely time to reboot the system. When I check task manager memory usage is almost 100% 
and the memory system seems to be **'thrashing'** and near complete collapse. 

Anti-virus security software over-zealous blocking of access to files seems to be a common problem
of memory management in the security rather than performance domain. 
Anti-virus software locking files and preventing access and use 
is one memory management problem that happens often. I brought a notebook a technician that was so slow in booting and performing
common tasks, that it was a big waste of time using it.   
I would usually reinstall Windows, but all the technician 
did was uninstall the Avast anti-virus app and the problem was solved. 
The task manager also notes the excessive memory usage of this program. 

Recently, the Avast anti-virus 
software I have installed on my system locked certain executable images making it 
impossible to install programs
For example, I wished to reinstall the OB screen video capture software used to 
make programming tutorials.   
Solving such issues and problems seems to require keeping updated 
on the latest system performance issues via a news feed, 
not necessarily deep knowledge of system internals.

### Linux Memory Management 

The open source programming tools I use are normally associated with Linux. On Windows. Docker did not even work properly on Windows until relatively recently, and deep learning experts recommend the use of a Linux machine for running deep learning models under PyTorch with a GPU. The biggest factor in using Linux for these programming tasks seems to be the openness of the operating system, increased certainty of what is happening in the OS, particularly for testing and ascertaining the nature of a bug when a program crashed. 

Windows experts are clear about the difficulty of ascertaining what is happening under the hood in Windows. This certainly stems from the closed proprietary nature of the source code, but also from a policy of not adding too much transparency and error-tracing capability to the system because this means adding extra overhead with a potential negative impact on performance (Windows memory management mystery YouTube).

Running open source programming tools under Linux, I only encounter a 
memory management problem when I am processing very large files. 
Then, I remember what I was taught about embedded loops with respect to time and space complexity of programs, and try to make the program as efficient as possible.

Of course, adding more memory, which is not that expensive, is the best way to improve memory management. This is in essence removing the need to 'manage' memory because there is less or even no memory scarcity at all. This is stressed repeatedly in one popular Linux internals video. 

I still have to purchase a GPU and start running Large Language Models (LLMs) on my personal machine, but when I do I am planning to do so under WSL, or set up a completely independent Linux machine. At that time, familiarity with NVidia's CUDA GPU management framework, which includes memory and cache management,  will  no doubt prove useful in order to tune it and optimize performance. 

### Uses of Memory Management Knowledge 

In depth knowledge of memory management seems to be most useful in certain well-defined areas.  
This includes performance tuning for different kinds of systems including GPU and cloud computing systems. 
Embedded system design and system security measures also seem to demand extensive detailed knowledge of memory management. 

I will devote the rest of this short paper to system performance tuning for GPUs and Docker containers. 

### Docker Memory Management

Infrastructure as Service Cloud computing platforms such as AWS, GCP, and MS Azure
and like gigantic bazaars with a wide variety of computing resources 
for sale at different prices. The first question to ask is whether 
one can get a better deal by varying patterns of resource usage 
over time and place. Namely, are there off-hours discounts, 
and are certain resources (or 'places') offered at different prices,
This is most obviously true for storage with fast vs. slow access times. 
Different types of archival storage, for instance, have low access privileges 
but very low price as well. 

The second question, is whether such computing cost
minimization opportunities are avaiable for docker containers running on the cloud. 

### GPU CUDA Memory Management 



### REFERENCES 

[Docker reference]

[Windows memory management mysteries Two part videos]

 

  

  

 



 






