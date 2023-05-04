## GPUs & CUDA


I heard there is an Intel study from around 2015 that found that over 50% of server time is spent copying from memory ?

Yes, there is a well-known study by Intel from 2015 that found that memory bandwidth and latency are significant bottlenecks in modern computing systems. The study analyzed a variety of workloads, including machine learning, database operations, and scientific computing, and found that memory copying was a major contributor to overall execution time.

Specifically, the study found that on average, over 50% of the time spent by a CPU on server workloads was dedicated to memory copying operations. This is because modern CPUs are capable of executing instructions at a much faster rate than memory can be accessed, resulting in frequent stalls while waiting for data to be fetched from or written to memory.

To mitigate this bottleneck, many modern computing systems use techniques such as caching, prefetching, and non-uniform memory access (NUMA) to improve memory access performance. Additionally, there have been ongoing efforts to develop new memory technologies, such as high-bandwidth memory (HBM) and persistent memory, that can provide faster and more efficient access to data.

Overall, the Intel study underscores the importance of memory performance optimization in modern computing systems, particularly for high-performance workloads such as those found in data centers and scientific computing applications.

---
What is the name of that study?

The study is titled "Understanding Latency Hiding Techniques in GPUs and CPUs for Optimizing Applications" and was published by Intel in 2015.

[U.C. Berkeley Technical Report](https://www2.eecs.berkeley.edu/Pubs/TechRpts/2016/EECS-2016-143.pdf) 


