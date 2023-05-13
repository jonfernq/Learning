## Traceroute command

The Linux traceroute command is used to trace the route that a packet takes to reach a destination host on the network. It works by sending packets with increasing Time to Live (TTL) values and recording the responses from the intermediate routers. This way, it can show the path and the delay of each hop¹²³.

The traceroute command can be useful for troubleshooting network connectivity issues, identifying network bottlenecks, and measuring network latency. You can also use various options and switches with traceroute command to customize your tests and get more information¹²³..

Source: Conversation with Bing, 5/13/2023
(1) How to Use the traceroute Command on Linux - How-To Geek. https://www.howtogeek.com/657780/how-to-use-the-traceroute-command-on-linux/.
(2) traceroute Command Examples in Linux. https://linuxhandbook.com/traceroute/.
(3) traceroute command in Linux with Examples - GeeksforGeeks. https://www.geeksforgeeks.org/traceroute-command-in-linux-with-examples/.

---
> I am using Ubuntu Linux and the traceroute command does not seem to be installed by default. Is the Tracepath command the same?

To install traceroute command on Ubuntu Linux, you can use the following command:

`sudo apt-get install traceroute`²⁴

This will install the traceroute package using the apt-get package manager. You may need to enter your password and confirm the installation.

Tracepath is a similar command to traceroute, but it does not require root privileges. It can also show the maximum transmission unit (MTU) of each hop along the path¹. However, tracepath may not work well with some firewalls or routers that block ICMP messages³. Therefore, traceroute may be more reliable and accurate in some cases. You can use both commands to compare the results and troubleshoot network issues.

Source: Conversation with Bing, 5/13/2023
(1) How to Install and Use Traceroute on Ubuntu. https://learnubuntu.com/traceroute/.
(2) how to install traceroute - Ask Ubuntu. https://askubuntu.com/questions/1178252/how-to-install-traceroute.
(3) How to install and use Traceroute on Ubuntu, Debian, AlmaLinux, Rocky .... https://www.layerstack.com/resources/tutorials/How-to-install-and-use-Traceroute-on-CentOS-Ubuntu-Debian-Cloud-Servers.
(4) How to Install and run Traceroute on Ubuntu 20.04 LTS. https://blog.eldernode.com/install-and-run-traceroute-on-ubuntu-20-04/.

---
> traceroute slashdot.org

![traceroute1](https://github.com/jonfernq/Learning/assets/68504324/c1353bc6-29a5-45d3-9600-de372067e4b2)

 Connecting from our machine to slashdot.org requires traversing 8 routers.
 
---

> What does a typical line in the 'traceroute' command output mean? 
> For example: " 2  192.168.100.1 (192.168.100.1)  2.725 ms  2.714 ms  2.716 ms"

A typical line in the traceroute command output means the following:

- The first number is the hop count, which indicates how many routers or gateways the packet has passed through to reach the destination. In this case, it is 2, which means this is the second hop.
- The second part is the hostname and the IP address of the router or gateway that received and forwarded the packet. In this case, it is 192.168.100.1, which is a private IP address that belongs to a local network.
- The third part is the round-trip time (RTT) of each packet sent to that hop, measured in milliseconds (ms). In this case, three packets were sent, and their RTTs were 2.725 ms, 2.714 ms, and 2.716 ms respectively. This shows how long it took for each packet to travel from the source to the hop and back. The lower the RTT, the faster the connection.

---

> What is the meaning of longer 'traceroute' output lines with multiple IP addresses ? example: " 5  202.137.133.209 (202.137.133.209)  30.274 ms 202.137.133.217 (202.137.133.217)  44.166 ms 202.137.133.254 (202.137.133.254)  36.254 ms"

A longer traceroute output line with multiple IP addresses means that there are multiple routers or gateways at the same hop that can forward the packet to the next hop. In this case, it means that the fifth hop has three possible paths to reach the sixth hop. Each IP address represents a different router or gateway at the same hop, and each RTT represents the time it took for the packet to reach that router or gateway and come back.

This can happen when there is load balancing or redundancy in the network, which means that there are multiple routes available to distribute the traffic or to provide backup in case of failure. Traceroute may choose a different path for each packet, depending on the network conditions and routing protocols. Therefore, you may see different IP addresses and RTTs for each packet at the same hop..

---



