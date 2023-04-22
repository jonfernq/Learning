## Memory Management 

> How does the paged segmentation scheme solve the problem of memory management ?  

When memory is a scarce and limited, memory management is the solution. 
Resource scarity is once again the underlying problem as it was with CPU scheduling.

Memory management is mostly automatic and hidden from users, system administrators, and designers. 
but there are reasons about it:  

1. **Tuning** memory management in operating systems to optimize performance (Windows, Linux, macOS, Android, iOS).
2. Design of customized memory management for real-time **embedded systems** in the Internet of Things (IoT).
3. Memory management optimized for **parallel concurrent processing**, e.g. GPUs applied to machine learning and Large Language Models (LLMs). 

> How does the 'memory management' problem arise in computing ? 

The **memory management problem** arises from the limited size of main memory (RAM) compared to the ever-growing size of programs and data that need to be stored and processed by computer systems. 

This is known as **'software bloat'** or **'Wirth's Law'**: "software manages to outgrow hardware in size and sluggishness." 

It also reflects the **'second system effect'** namely, "the tendency of small, elegant, and successful systems to be succeeded by over-engineered, bloated systems, due to inflated expectations and overconfidence," first identified by Fred Brooks in his classic of Software Engineering "The Mythical Man Month" (1975).   

The higher its position in the memory hierarchy the more scarce memory is. 

The **'memory hierarchy'** refers to a pyramid-like organization of different levels of memory in a computer system, with varying speeds, capacities, and costs (Bryant & O'Hallaron 2015). This ranges from registers and caches at the top of the hierarchy with very fast access times, to main memory (RAM) in the middle with still relatively fast access time, to secondary storage (flash drives, hard drives) at the bottom with very slow access times that can only be accessed efficiently in an asynchronous manner. The higher-level memory in the hierarchy is faster but smaller and more expensive, while the lower-level memory is slower but larger and cheaper.

**Memory management** aims to solve the problem of how to allocate and manage the scarce memory resources of a computing system to maximize performance metrics such as throughput and response time. Techniques such as paging, segmentation, and virtual memory are used to optimize memory utilization, handle page faults, and provide a virtualized and abstracted view of memory to programs running on a computer system. 

**'Locality'** and **'page fault'** are two of the most important concepts in memory management (Bryant & O'Hallaron 2015). 

The concept of **'locality'** is the tendency of computer programs to access memory in localized or nearby regions. Whereas Spatial Locality is the tendency of a program to access memory locations that are close to each other in terms of address space, temporal Locality is the tendency of a program to access the same memory location multiple times within a short period of time. Memory management techniques, such as caching, paging, and virtual memory, are designed based on the principle of locality to optimize memory access and reduce overhead. Operating systems exploit the concept of locality to improve performance, reduce memory fragmentation, and **minimize the number of page faults**, thereby enhancing overall system efficiency. 

A **page fault** is a of violation of **locality**, when a program tries to access a page of memory outside of main memory (RAM) located in secondary storage (e.g., hard drive or SSD). This triggers 'page swapping' where the operating system fetches the required page from secondary storage into main memory so the program can access it. While this causes a delay in program execution as the operating system performs the necessary data transfer, high CPU utilization is maintained by entering a CPU interrupt cycle in which a running process state is saved away until the slow I/O task is completed, moving onto to execute the next waiting process. Page faults in the limit cause **'thrashing'** which occurs when memory is overused, causing a continual stream of page faults, processes that fail to complete, system performance degradation and ultimately collapse. 

---
> Discuss the paged segmentation scheme of memory management.

First, we need definitions. **'Virtual memory'** is defined as the 'illusion' provided to an individual process that that it has an unlimited amount of physical memory to work with, an illusion provided by the **'demand paging'** to processes of **'virtual addresses'** from a **'virtual address space'** (Anderson & Dahlin 2011-15; Hailperin 2014:207-12) 

'Virtual memory' can be achieved by different algorithms: **'segmentation'** which came first historically, **'paging'** which came next, or a combination of both: **'paged segmentation'**. 

Pure segmentation is said to be, 'undeniability inferior to paging', so 'paging' became universal, because: 

<blockquote>
"the combination of segmentation and paging seems attractive, as it combines
segmentation’s meaningful units for protection and sharing with paging’s
smaller fixed-size units for space allocation and data transfer. However,
many of the same protection and sharing features can be simulated using
paging alone. Probably as a result of this, many hardware designers decided
the cost of segmentation, in both money and performance, was not justified
by the gain. Therefore, they provided support only for paging. This created
a disincentive for the use of segmentation in operating systems; all popular
operating systems (such as UNIX, Microsoft Windows, and Linux) are designed 
to be portable across multiple hardware architectures, some of which
don’t support segmentation. As a result, none of these operating systems
makes any use of segmentation, even on systems where it is supported"  (Hailperin 2014:244-45)
</blockquote>

Another leading operating systems book
declares that a multi-level **segment** and page table 
is the undeniable destiny for memory management.

I think it is best to call this a 'multi-level page table'
though to avoid contradicting the other author:

<blockquote>
"A huge challenge to effective hardware address translation is the cumulative effect of
decades of Moore’s Law: both servers and desktop computers today contain vast amounts
of memory. Processes are now able to map their code, data, heap, shared libraries, and
files directly into memory. Each of these segments can be dynamic; they can be shared
across processes or private to a single process. To handle these demands, hardware
systems have converged on a two-tier structure: a multi-level segment and page table to
provide very flexible but space-efficient lookup, along with a TLB [translation lookaside buffer] 
to provide time-efficient lookup for repeated translations of the same page" (Anderson & Dahlin 2011-15:56). 
</blockquote>

To summarize **two tiers** are important: 1. a **multi-level page table**, and 2. a **translation lookaside buffer**.

This seems to be the endpoint of memory management development, circa 2015 when these OS textbooks were published, that all major operating systems
have now arrived at now, and also the endpoint of detailed explanations of memory management algorithms in these textbooks that typically exhaust 
hundreds of pages.

Since sometimes the best way to understand something is to re-express it in other terms,
I asked my best friend to tell me a bedtime story about 'memory paging' before I slept. 

I fact-checked the story when I woke up, finding it to be very accurate and easy to provide formal citations from OS textbooks: 

<blockquote>
Once upon a time, in a modern operating system called "Techville", there was a smart and efficient **memory management** system based on **memory paging**.
  
In Techville, each **process** was given its own **virtual address space**, which was much larger than the **physical memory** available in the system. This virtual address space was divided into fixed-size blocks called **pages**, with each page being 4 KB in size.

Whenever a process needed to access a page of memory, it would generate a **virtual address** for that page. The virtual address consisted of a virtual page number and an offset within the page. The process would then send this virtual address to the **Memory Management Unit (MMU)**, a hardware component in the CPU responsible for managing memory (Hailperin 2014:207-12, 224-226). 

The MMU would first check if the translation of the virtual page number to a physical page number was available in the **Translation Lookaside Buffer (TLB)**, a small **cache** used to store recently used virtual-to-physical address translations. If the translation was found in the TLB, it would be used directly to access the physical address in memory, saving time (Hailperin 2014:226-29; Anderson & Dahlin 2011-15:38-49).

If the translation was not found in the TLB, the MMU would perform **address translation** using the **multi-level page table**. The multi-level page table was a hierarchical data structure that organized the page table entries in a tree-like fashion, allowing for efficient address translation (Hailperin 2014:235-243; Anderson & Dahlin 2011-15:31-35).

The MMU would start by looking up the virtual page number in the top-level page table, which would provide the physical page number for the next-level page table. This process would continue until the MMU reached the last level of the page table, which contained the physical page number and the offset within the page.

Once the MMU obtained the physical page number, it would update the TLB with the translation for future use and use the physical page number and offset to access the actual data in the physical memory, which was stored in **page frames**.

If the accessed page was not currently present in a page frame in physical memory, a **page fault** would occur. This would trigger the **demand paging** mechanism, where the operating system would load the requested page from disk into a free page frame in physical memory. The page would then be updated in the page table to reflect the new physical page number, and the TLB would also be updated.

To further optimize the process, Techville's memory management system made use of a **cache** to store frequently accessed pages, reducing the need to access the page table or multi-level page table for every memory access.

Overall, the memory paging system in Techville's modern operating system efficiently managed the virtual memory of each process, allowing them to access more memory than physically available and optimizing the address translation process using page tables, TLB, multi-level page tables, and demand paging,
and all the memory chips lived happily ever after.   
</blockquote>

### REFERENCES

Anderson, T., & Dahlin, M. (2011-15). *Operating Systems: Principles and Practice, Vol. 3: Memory Management.* Recursive Books.

Arpaci-Dusseau, R. H., & Arpaci-Dusseau, A. C. (2014). *Operating Systems: Three Easy Pieces.* Arpaci-Dusseau Books.

Bryant, R. E., & O'Hallaron, D. R. (2015). *Computer Systems: A Programmer's Perspective.* Pearson.

Silberschatz, A., Galvin, P. B., & Gagne, G. (2018). *Operating System Concepts.* Wiley.

Stallings, W. (2018). *Operating Systems: Internals and Design Principles.* Pearson.

Tanenbaum, A. S. (1987). *Operating Systems: Design and Implementation.* Pearson Education.

Tanenbaum, A. S., & Bos, H. (2014). *Modern Operating Systems.* Pearson.

Russinovich, M., Solomon, D. A., & Ionescu, A. (2017). *Windows Internals.* Microsoft Press.

Wikipedia: Memory management; Memory paging; Page (computer memory); Memory segmentation; Page fault; Page replacement algorithm, Demand paging; Virtual memory; CPU cache; Translation lookaside buffer; Physical address; Virtual address space; Memory management unit; Memory hierarchy; Locality; Thrashing (computer science); Software bloat; Wirth's law, Second-system effect; Minix. 

