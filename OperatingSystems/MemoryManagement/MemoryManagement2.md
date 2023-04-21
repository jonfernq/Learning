## Memory Management 

> How does the paged segmentation scheme solve the problem of memory management ? 

Resource scarity is once again the underlying problem as it was in the last unit where we studied CPU scheduling. When memory is a scarce and limited resource, memory management is the solution.  

The basic concepts of memory management are simple, but the details of variant implementations and their historical development are complex. These concepts can be put to practical use in three ways:  

1. Tuning memory management on operating systems in common use (Windows, Linux, macOS, Android, iOS) to optimize performance.
2. Design of customized memory management for real-time embedded systems in the Internet of Things (IoT).
3. Memory management optimized for parallel processing, for example GPUs applied to machine learning and Large Language Models (LLMs). 

> How does the 'memory management' problem arise in computing ? 

The memory management problem arises historically from the limited size of main memory (RAM) compared to the ever-growing size of programs and data that need to be stored and processed by computer systems. This phenomenon is known as 'software bloat' and Wirth's Law: "software manages to outgrow hardware in size and sluggishness." This also reflects the 'second system effect' namely, "the tendency of small, elegant, and successful systems to be succeeded by over-engineered, bloated systems, due to inflated expectations and overconfidence," first identified by Fred Brooks in his classic of Software Engineering "The Mythical Man Month" (1975).   

The higher its position in the memory hierarchy, the more scarce memory is. The **'memory hierarchy'** refers to a pyramid-like organization of different levels of memory in a computer system, with varying speeds, capacities, and costs (Bryant & O'Hallaron 2015). This ranges from registers and caches at the top of the hierarchy with very fast access times, to main memory (RAM) in the middle with still relatively fast access time, to secondary storage (flash drives, hard drives) at the bottom with very slow access times that can only be accessed efficiently in an asynchronous manner. The higher-level memory in the hierarchy is faster but smaller and more expensive, while the lower-level memory is slower but larger and cheaper.

**Memory management** aims to solve the problem of how to allocate and manage the scarce memory resources of a computing system to maximize performance metrics such as throughput and response time. Techniques such as paging, segmentation, and virtual memory are used to optimize memory utilization, handle page faults, and provide a virtualized and abstracted view of memory to programs running on a computer system. 

'Locality' and 'page fault' are two of the most important concepts in memory management (Bryant & O'Hallaron 2015). The concept of **'locality'** is the tendency of computer programs to access memory in localized or nearby regions. Whereas Spatial Locality is the tendency of a program to access memory locations that are close to each other in terms of address space, temporal Locality is the tendency of a program to access the same memory location multiple times within a short period of time. Memory management techniques, such as caching, paging, and virtual memory, are designed based on the principle of locality to optimize memory access and reduce overhead. Operating systems exploit the concept of locality to improve performance, reduce memory fragmentation, and **minimize the number of page faults**, thereby enhancing overall system efficiency. 

A **page fault** is in essence a of violation of locality, occuring when a program tries to access a page of memory outside of main memory (RAM) but rather in secondary storage (e.g., hard drive or SSD). This triggers 'page swapping' where the operating system fetches the required page from secondary storage into main memory so the program can access it. Page faults cause a delay in program execution as the operating system performs the necessary data transfer. This involves the CPU interrupt cycle in which a running process state is saved away until a slow I/O task is completed to ensure CPU utilization is maximized.  

---
> Discuss the paged segmentation scheme of memory management.

The paged segmentation scheme is a memory management scheme that combines aspects of both paging and segmentation, which are two popular memory management techniques used in operating systems. In the paged segmentation scheme, the logical address space of a process is divided into segments, and each segment is further divided into pages. This allows for flexible and efficient memory allocation and address translation.

Component parts of paged segmentation scheme:

- Segments: In this scheme, the logical address space of a process is divided into segments, which are portions of the process's address space that represent different parts of the program, such as code, data, and stack. Each segment is assigned a segment number.

- Pages: Each segment is further divided into fixed-size pages. Pages are the smallest units of memory that can be managed by the paging system. Pages are typically of the same size as the pages used in pure paging schemes, and they are assigned a page number within their segment.

- Page Table: For each segment, there is a page table that maps the segment's logical addresses to physical addresses in the main memory. The page table contains entries that map segment numbers and page numbers to physical frame numbers, which represent the corresponding locations in the physical memory where the pages are stored.

- Address Translation: When a process generates a logical address, it is divided into a segment number and a page number. The segment number is used to index the segment's page table entry, which contains the base address of the segment in the physical memory. The page number is used to index the page table entry, which contains the physical frame number where the page is stored. The base address of the segment and the physical frame number are added to the offset of the logical address within the page to generate the physical address in the main memory.

- Paging and Swapping: Pages in the main memory are managed using the same techniques as in pure paging schemes, such as swapping pages in and out of the main memory to the secondary storage (e.g., disk) as needed, to manage the limited physical memory efficiently.

The paged segmentation scheme allows for flexible memory allocation, as segments can be of different sizes, and each segment can have its own page table, allowing for efficient address translation. It also provides protection between segments, as each segment can have its own set of permissions and access rights. However, the paged segmentation scheme may suffer from fragmentation, both external and internal, as segments and pages may not always be fully utilized, leading to wasted memory. Therefore, careful management of segments and pages is required to minimize fragmentation and optimize memory utilization in a system using the paged segmentation scheme.

---
> Explain how a logical address is converted into a physical address in such a scheme.

In the paged segmentation scheme of memory management, the conversion of a logical address to a physical address involves several steps. Here's an overview of the process:

- Segment Number Extraction: The logical address generated by a process typically consists of a segment number and a page number. The segment number is extracted from the logical address, usually by using a fixed number of bits or by using a specific addressing mode.

- Segment Table Lookup: Once the segment number is extracted, it is used as an index to look up the corresponding entry in the segment table. The segment table is a data structure that stores the mapping between segment numbers and the base addresses of the corresponding segments in the physical memory. The segment table is typically maintained by the operating system.

- Base Address Addition: The base address of the segment, obtained from the segment table, is added to the page number to calculate the base address of the page within the segment. This step effectively translates the segment number into a physical memory address.

- Page Table Lookup: Next, the page number is used as an index to look up the corresponding entry in the page table associated with the segment. The page table is a data structure that stores the mapping between page numbers and the physical frame numbers where the pages are stored in the main memory.

- Physical Address Calculation: The physical frame number obtained from the page table is combined with the offset portion of the logical address (which represents the position of the desired data within the page) to calculate the physical address in the main memory. The offset is added to the base address of the page within the segment to obtain the final physical address.

- Data Access: The physical address is then used by the memory management unit (MMU) in the hardware to access the data or instruction stored in the corresponding physical memory location.

---
> What are examples of operating systems with paged segmentation scheme of memory management.

Virtually every operating system uses paged segmentation scheme of memory management that only differs in implmentation: 

- Windows: Windows uses a combination of paging and segmentation to manage virtual memory. The segment descriptors in Windows contain information such as the base address, limit, and access rights for each segment. The paging mechanism provides fine-grained control over the allocation and deallocation of pages, while the segmentation mechanism provides memory protection and isolation between processes. Windows also uses a Translation Lookaside Buffer (TLB) to cache the page table entries for faster address translation.

- Linux: Linux also uses a combination of paging and segmentation for memory management. In Linux, segments are represented as virtual memory areas (VMAs) and are managed by the Memory Management Unit (MMU) in the processor. Linux uses a hierarchical page table structure with multiple levels of page tables for efficient address translation. Linux also supports features such as copy-on-write, memory sharing, and memory-mapped files, which are implemented using the paged segmentation scheme to achieve desired behavior and coordination in concurrent systems.

- macOS: macOS also uses a combination of paging and segmentation for virtual memory management. Segments in macOS are represented as memory objects and are managed by the Mach Virtual Memory System. macOS uses a two-level page table structure for address translation, with a global page table for shared memory and a per-process page table for private memory. macOS also supports memory sharing, copy-on-write, and other memory management features implemented using the paged segmentation scheme.

- Android: Android also uses a combination of paging and segmentation for virtual memory management. Android uses the Linux kernel for memory management, including the paged segmentation scheme. Android supports multiple processes and apps running on the system, and the paged segmentation scheme is used to provide memory protection and isolation between these processes and apps. Android also supports memory sharing, copy-on-write, and other memory management features implemented using the paged segmentation scheme.

System administrators or designers can tune memeory management by configuring parameters related to virtual memory, such as page file size, page replacement algorithms, TLB size, and page table structure, to optimize system performance based on specific workload requirements. They can also set memory-related parameters for processes or applications, such as memory limits, priority, and sharing options, to manage memory usage effectively.

### REFERENCES

Anderson, T., & Dahlin, M. (2011-15). *Operating Systems: Principles and Practice, Vol. 3: Memory Management.* Recursive Books.

Arpaci-Dusseau, R. H., & Arpaci-Dusseau, A. C. (2014). *Operating Systems: Three Easy Pieces.* Arpaci-Dusseau Books.

Bryant, R. E., & O'Hallaron, D. R. (2015). *Computer Systems: A Programmer's Perspective.* Pearson.

Silberschatz, A., Galvin, P. B., & Gagne, G. (2018). *Operating System Concepts.* Wiley.

Stallings, W. (2018). *Operating Systems: Internals and Design Principles.* Pearson.

Tanenbaum, A. S. (1987). *Operating Systems: Design and Implementation.* Pearson Education.

Tanenbaum, A. S., & Bos, H. (2014). *Modern Operating Systems.* Pearson.

Russinovich, M., Solomon, D. A., & Ionescu, A. (2017). *Windows Internals.* Microsoft Press.

Wikipedia: Memory management; Memory hierarchy; Locality; Software bloat; Wirth's law, Second-system effect; Memory paging; Page fault; Page replacement algorithm, Demand paging, Memory segmentation, Thrashing (computer science); Minix. 

