
> What are the criteria necessary for an effective and efficient network?

Performance, reliability, and security are three important criteria for an effective and efficient network that meets the requirements and expectations of users and applications.

Some of these criteria are shared by the design goals of 'network protocols', the rules that govern data transmission in a network (Hailperin 2019:298), as listed in Tannenbaum (2021:47):

1. Reliability - the ability to recover from errors, faults, or failures
2. Resource allocation - sharing access to a common, limited resource);
3. Evolvability - incremental deployment of protocol improvements over time
4. Security - defending the network against various types of attacks

The important criteria of reliability and security will be addressed in this section and performance in the next.

Reliability is the degree to which a network is dependable and consistent in data transmission. It includes factors such as availability, scalability, fault tolerance, and redundancy of the network.

Availability is the percentage of time that the network is operational. Scalability is the ability to handle increased workloads.

Fault tolerance is the ability of the network to recover from failure and errors.

Redundancy consists of alternative backup service provision when there is network failure.

Security is the degree of protection from intrusion and interference from outside the network (Comer 2015:506-31).

Security includes preventing unauthorized access, modification, or destruction of data.
Factors that affect security include authentication, authorization, encryption, integrity, and non-repudiation of the network.

Authentication is the process of verifying the identity of a user or device before granting them access to the network.

Authorization is the process of granting or denying permissions to a user or device based on assigned roles and privileges.

Encryption is encrypting data to protect privacy.

Integrity guarantees that data is not altered or corrupted during transmission or storage.

Non-repudiation is the process of ensuring that a sender or receiver cannot deny sending or receiving a message.


> What are the specific factors that affect the performance of a network?

Performance is the degree to which a network delivers data accurately in a timely fashion, and is determined by factors such as network bandwidth, latency, throughput, jitter, and error rate (Comer 2015:469-504).

Bandwidth is the capacity of a network to transmit data. The higher the bandwidth, the faster data can be transmitted and the more data can be transmitted during a fixed period of time. Bandwidth differs by transmission media: twisted copper wire, fiber-optic cable, or wireless (Wi-Fi).

Network latency is the delay in transmitting data which can increase with network congestion.

Whereas bandwidth is the amount of data that can be transmitted per unit of time, throughput is the rate of data transmission.

Whereas latency is the delay between sending and receiving data, jitter is the variation in latency over time.

Error rate is the percentage of data packets corrupted, lost, or dropped during transmission.

(MBA Knowledge Base n.d.; BBC n.d.)


> How can any network be affected by one or a combination of these factors? or:
How can the above factors combine to make network performance and functioning worse?

Latency (delay) and throughput (capacity) are tightly coupled and dependent on each other. The end-user experience of different systems is often dominated by either throughput or latency limitations:

"Bandwidth and latency combine to define the performance characteristics of a given link or channel. Their relative importance, however, depends on the application." (Peterson &  Davie 2022:46).

Different types of network traffic have different bandwidth and latency requirements, such as streaming video, voice calls, web browsing, or file transfers. User experience can degrade significantly in applications such as gaming and video conferencing when latency requirements are not met.

For a given bandwidth and latency, the more devices on the network, the more bandwidth and resources they consume, which reduces the speed and availability of the network for other devices.

A bandwidth bottleneck arises when there is not enough bandwidth for the devices using a network, the network cannot handle data transmission demand resulting in slow service with interruptions, poor service, and users whose expecations have not been met.

Retransmission after failure increases network load, latency, and decreases performance. When the network has a high level of noise, interference, collisions, or hardware failures, it can cause high error rate and retransmission, which means that the network has frequent and severe data corruption or loss. This reduces accuracy, reliability, and increases network load and latency, due to retransmission of corrupted lost data packets.

REFERENCES

BBC (n.d.). "Wired and wireless networks." BITESIZE. Retrieved May 12, 2023, from https://www.bbc.co.uk/bitesize/guides/zvspfcw/revision/8

Comer, D. (2015). Computer networks and internets. Sixth Edition. Cambridge, MA, USA:: Pearson.

Hailperin, M. (2019). Operating systems and middleware: Supporting controlled interaction. Thomson Learning, Inc.: San Francisco, CA .

MBA Knowledge Base (n.d.). "Wired and wireless networks." Retrieved May 12, 2023, from https://www.mbaknol.com/information-systems-management/factors-affecting-the-performance-of-computer-networks/

Peterson, L. L., & Davie, B. S. (2022). Computer networks: a systems approach. Sixth Edition. Elsevier.

Tanenbaum, A. S., Feamster, N. & Weatherall, D. (2021). Computer networks. Sixth Edition. Pearson Education.

Wikipedia: Network performance, Latency, Throughput, Communication protocols, Latency (engineering), Network throughput






> What are the criteria necessary for an effective and efficient network?

Performance, reliability, and security are three important criteria for an effective and efficient network that meets the requirements and expectations of users and applications. 

Some of these criteria are shared by the design goals of 'network protocols', the rules that govern data transmission in a network (Hailperin 2019:298), as listed in Tannenbaum (2021:47): 

1. Reliability - the ability to recover from errors, faults, or failures 
2. Resource allocation - sharing access to a common, limited resource); 
3. Evolvability - incremental deployment of protocol improvements over time 
4. Security - defending the network against various types of attacks
 
The important criteria of reliability and security will be addressed in this section and performance in the next. 

Reliability is the degree to which a network is dependable and consistent in data transmission. It includes factors such as availability, scalability, fault tolerance, and redundancy of the network. 

Availability is the percentage of time that the network is operational. Scalability is the ability to handle increased workloads.

Fault tolerance is the ability of the network to recover from failure and errors.

Redundancy consists of alternative backup service provision when there is network failure.

Security is the degree of protection from intrusion and interference from outside the network (Comer 2015:506-31). 

Security includes preventing unauthorized access, modification, or destruction of data. 
Factors that affect security include authentication, authorization, encryption, integrity, and non-repudiation of the network. 

Authentication is the process of verifying the identity of a user or device before granting them access to the network. 

Authorization is the process of granting or denying permissions to a user or device based on assigned roles and privileges. 

Encryption is encrypting data to protect privacy. 

Integrity guarantees that data is not altered or corrupted during transmission or storage. 

Non-repudiation is the process of ensuring that a sender or receiver cannot deny sending or receiving a message.


> What are the specific factors that affect the performance of a network? 

Performance is the degree to which a network delivers data accurately in a timely fashion, and is determined by factors such as network bandwidth, latency, throughput, jitter, and error rate (Comer 2015:469-504). 

Bandwidth is the capacity of a network to transmit data. The higher the bandwidth, the faster data can be transmitted and the more data can be transmitted during a fixed period of time. Bandwidth differs by transmission media: twisted copper wire, fiber-optic cable, or wireless (Wi-Fi). 

Network latency is the delay in transmitting data which can increase with network congestion. 

Whereas bandwidth is the amount of data that can be transmitted per unit of time, throughput is the rate of data transmission. 

Whereas latency is the delay between sending and receiving data, jitter is the variation in latency over time.  

Error rate is the percentage of data packets corrupted, lost, or dropped during transmission.

(MBA Knowledge Base n.d.; BBC n.d.)


> How can any network be affected by one or a combination of these factors? or:
How can the above factors combine to make network performance and functioning worse?

Latency (delay) and throughput (capacity) are tightly coupled and dependent on each other. The end-user experience of different systems is often dominated by either throughput or latency limitations:

"Bandwidth and latency combine to define the performance characteristics of a given link or channel. Their relative importance, however, depends on the application." (Computer Networks a Systems Approach p. 46) 

Different types of network traffic have different bandwidth and latency requirements, such as streaming video, voice calls, web browsing, or file transfers. User experience can degrade significantly in applications such as gaming and video conferencing when latency requirements are not met.

For a given bandwidth and latency, the more devices on the network, the more bandwidth and resources they consume, which reduces the speed and availability of the network for other devices. 

A bandwidth bottleneck arises when there is not enough bandwidth for the devices using a network, the network cannot handle data transmission demand resulting in slow service with interruptions, poor service, and users whose expecations have not been met.

Retransmission after failure increases network load, latency, and decreases performance. 
When the network has a high level of noise, interference, collisions, or hardware failures, it can cause high error rate and retransmission, which means that the network has frequent and severe data corruption or loss. This reduces accuracy, reliability, and increases network load and latency, due to retransmission of corrupted lost data packets.

**REFERENCES** 

BBC (n.d.). Wired and wireless networks. BITESIZE. Retrieved May 12, 2023, from https://www.bbc.co.uk/bitesize/guides/zvspfcw/revision/8

Comer, D. (2015). Computer networks and internets. Sixth Edition. Cambridge, MA, USA:: Pearson.

Hailperin, M. (2019). Operating systems and middleware: Supporting controlled interaction. Thomson Learning, Inc.: San Francisco, CA .

MBA Knowledge Base (n.d.). Wired and wireless networks. Retrieved May 12, 2023, from https://www.mbaknol.com/information-systems-management/factors-affecting-the-performance-of-computer-networks/

Peterson, L. L., & Davie, B. S. (2022). Computer networks: a systems approach. Sixth Edition. Elsevier.

Rappaport, Theodore S. (2002). Wireless communications : principles and practice (2 ed.). Upper Saddle River, NJ: Prentice Hall PTR. ISBN 0-13-042232-0.

Tanenbaum, A. S., Feamster, N. & Weatherall, D. (2021). Computer networks. Sixth Edition. Pearson Education.

Wikipedia: Network performance, Latency, Throughput, Communication protocols, Latency (engineering), Network throughput    





