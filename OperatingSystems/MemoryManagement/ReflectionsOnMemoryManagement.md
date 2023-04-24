## Memory Management Issues

> Analyze an operating system’s memory management. The report must convey the key areas of memory management-hardware memory management, 
operating memory management, and application memory management. The content should also explain problems relating to memory management. 
The conclusion should include which operating system is the best for your computer and recommend solutions to memory management problems. 
Images and tables are encouraged but should not constitute more than one page of the report.

The conclusion of this paper will include: a choice of operating system with special attention paid to memory management, 
and solutions to memory management problems encountered in the past. 
The hardware, operating system, and application levels of memory management will all be addressed. 

In my discussion write-up I have already dealt with memory management theory, largely hidden from user view, I won't address that here. 

This report compares two operating systems: 1. Windows 10 the main OS on my notebook, and 2. Ubuntu Linux running under WSL on Windows 10. 

I have used Windows 10 on notebooks and desktops to perform everyday work and tasks for decades. 

However, it is clear that Linux is the best platform for the open source programming languages and frameworks I use: Python, PyTorch, JavaScript, React.js, and Docker.  

### Windows 10 Memory Management Experiences 

Memory is the key system resource that has caused past performance degradation on my Windows 10 notebook. 

Several years back, moving from slow hard drives to Solid State Drives (SSDs) 
led to huge improvements in performance.
SSDs are not as fast as memory, but stoarage capacity is much larger. 
I started purchasing notebooks with 256GB of SSD memory with 8 GB of RAM main memory,
providing enough capacity while still being affordable.   

Nowadays, the only real memory management problem under Windows 10 occurs 
when I open to many windows and tabs in the  Chrome browser.
Other memory intensive applications contribute to the system overload as well,
but Chrome browser memory usage always stands out in the task manager 
and the Chrome browser is known-to-be memory intensive. 

![taskmanger1](https://user-images.githubusercontent.com/68504324/233879364-c9cf236d-fb90-4c2c-852c-2fe1f6ac8de6.jpg)

![taskmanager2](https://user-images.githubusercontent.com/68504324/233879377-4b63fad3-10e9-4244-9c5f-a680d18e0ed8.jpg)

Instead of rebooting everytime I use the computer, I just put it to sleep in order to easily 
save my work environment, avoiding reset up time and remembering what I was doing. 
If one does this for many days, eventually there are obvious signs that memory is tied up 
and cannot be used, some apps will not open, one cannot print from the browser, and the wi-fi tab even disappears. 
Then it is definitely time to reboot the system. When I check task manager memory usage is almost 100% 
and the memory system seems to be **'thrashing'** and near complete collapse. 

Anti-virus security software locking files and not allowing them to be loaded into memory seems to be a common problem
of memory management in the security domain. 
Anti-virus software is always resident in memory and serves as a sort of gatekeeper to memory and the kernel,
but sometimes it seems to be over-zealous in performing this function. 
The standard advice on debugging and resolving these problems of course applies.

- Google the problem and error messages, particularly noting Stack Overflow suggestions.
- Read the Manual. 
- Review settings and confuigurations. 
- Check the logs.  

I brought a notebook to a technician that was so slow in booting and performing
tasks, that it was a big waste of time using it.   
Faced with such a situation I would usually just reinstall Windows, but all the technician 
did was *uninstall the Avast anti-virus software* and the problem was solved. 
The task manager also noted the excessive memory usage of this program. 

Recently, the Avast anti-virus 
software I have installed on my system locked certain executable images making it 
impossible to install important programs
For example, I wished to install video screen capture software used to 
make programming tutorials.  
It seemed to want to access executables related to video, but the anti-virus would not allow access.
The recommended workaround was temporarily disabling the anti-virus software.
This did not work, however, and a full uninstall and reinstall was needed. 
Solving such issues and problems seems to require keeping updated 
on the latest system performance issues via a news feed, 
not necessarily deep knowledge of system internals.

### Linux Memory Management Goals

The open source programming tools I use are normally associated with Linux. On Windows. Docker did not even work properly on Windows until relatively recently, and deep learning experts recommend the use of a Linux machine for running deep learning models under PyTorch with a GPU. The biggest factor in using Linux for these programming tasks seems to be the openness of the operating system, increased certainty of what is happening in the OS, particularly for testing and ascertaining the nature of a bug when a program crashed. 

Windows experts are clear about the difficulty of ascertaining what is happening under the hood in Windows. This certainly stems from the closed proprietary nature of the source code, but also from a policy of not adding too much transparency and error-tracing capability to the system because this means adding extra overhead with a potential negative impact on performance (Russinovich 2016).

Running open source programming tools under Linux, I only encounter a 
memory management problem when I am processing very large files. 
Then, I remember what I was taught about embedded loops with respect to time and space complexity of programs, and try to make the program as efficient as possible.

Of course, adding more memory, which is not that expensive, is the best way to improve memory management. This is in essence removing the need to 'manage' memory because there is less or even no memory scarcity at all. This is stressed repeatedly in one popular Linux internals video. 

I still have to purchase a GPU and start running Large Language Models (LLMs) on my personal machine, but when I do I am planning to do so under WSL, or set up a completely independent Linux machine. At that time, familiarity with NVidia's CUDA GPU management framework, which includes memory and cache management,  will  no doubt prove useful in order to tune it and optimize performance. 

### Uses of Memory Management Knowledge 

In depth knowledge of memory management seems to be most useful in certain well-defined areas.  
This includes performance tuning for cloud computing and GPUs, . 
as well as embedded system design and system security measures.  

I will devote the rest of this short paper to system performance tuning for cloud computing and leave the rest for later.

### Cloud Memory Management

Nowadays, one can launch all your applications in the cloud on virtual machines, containers,
or serverless applications. 

Infrastructure as A Service (IAAS) cloud computing platforms provide these services.
There are major providers such as AWS, GCP, and MS Azure,
and smaller ones such as Vercel, Digital Ocean, Netlify and many others.  

Like gigantic bazaars they offer a wide variety of computing resources 
at many different prices. 

On Amazon's AWS for intance, One can get a better deal by varying patterns of resource usage 
over time and place purchasing them on a spot market 
or by reserving them and committing up front to use and pay for resources (Wittig & Wittig 2018:92-100).  

#### Container Memory Management

In contrast to virtual machines (VMs) on the cloud,
flexibility memory allocation is a feature of Docker containers.  
By default, Docker containers will allocate as much memory from the 
host as possible, but limits on memory consumption can easily 
be placed on each container (Miell & Sayers 2019:386-88). 

Automated container orchestration systems such as Kubernetes
allow for more fine-grained control of containers (Comer 2021).
Using 'control theory' a 'control loop; continually monitors
the state of the system, which includes the important factor of 
memory usage, and makes adjustments to bring the actual state
to the desired state.  

Resource requirements and limits for containers are specified at configuration time.
Monitoring then ensures that containers have sufficient resources to run efficiently and reliably.

When pods that manage a microservice are created or updated, the control loop checks the requested memory resources of the containers in the pod against the available resources in the cluster. It ensures that the pod is scheduled to a worker node (virtual machine) that has enough available memory to meet the requested resources. If there is not enough available memory, the pod may be queued or rescheduled until sufficient resources become available.

The control loop also monitors the memory usage of containers and if limits are exceeded actions can be triggered. Autoscaling can also adjust the number of replicas of a deployment (replica set) based on memory usage patterns. 

So in conclusion, I would like to note that memory management nowadays extends well beyond one's machine. 
It is quite typical to use portable Dockers for software development on a Windows machine, for instance,
and then deploy that container in the cloud under Kubernetes where its resource usage can be autoscaled to meet 
resource needs. 

### REFERENCES 

Comer, D. (2021). *The Cloud Computing Book: The Future of Computing Explained.* CRC Press.

Hohn, Alan. (2022). *The Book of Kubernetes: A Complete Guide to Container Orchestration.* No Starch Press.

Miell, I., & Sayers, A. (2019). *Docker in practice.* Simon and Schuster.

Russinovich, Mark. (2016, December 19). 'Mysteries of Memory Management Revealed.' Part 1 of 2. WCL405 HD [Video]. YouTube.com. https://www.youtube.com/watch?v=TrFEgHr72Yg

Wittig, M., & Wittig, A. (2018). *Amazon web services in action.* Simon and Schuster.

