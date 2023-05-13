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

---




