Operating Systems - Week #6 - Networking 
Jon Fernquest 

PING 

A ping command tests the connectivity and performance of a host on a network.
One pings a host name or ip address, for example: ping 201.54.100.1 or ping www.google.com 

ICMP packets are used by the ping command to test the connectivity and performance of a host on a network.  
ICMP packets are first sent to a host. Then, the command listens for ICMP replies.  
Finally, the ping command shows you how long each packet took to make the round trip or tells you there was no reply. 
A ping command can be used to:

- Discover if a host is up and running.
- Check to see if a webserver is up and running (TCP ping).
- Network performance analysis.
- Troubleshoot network problems.
- Check if network  elements are working properly and how fast they are responding (including interface cards, cabling, routing, and gateways).

Most network devices will reply to these packets by default, unless they are configured otherwise. 
Some network devices may ignore or block ICMP packets for security reasons, 
such as hiding their presence or preventing denial-of-service attacks. 
Firewalls may also filter ICMP packets to protect the network from unwanted traffic.


> The Ping for IP address 201.54.100.1 never returns. 

This is an IP address, assigned to each device connected to a computer network that uses the Internet Protocol for communication. 
It serves two main functions: host or network interface identification and location addressing¹.

This IP address belongs to the Brazil country code and the Sao Paulo region. 
It is associated with the Telesp internet service provider.

![100percentloss](https://github.com/jonfernq/Learning/assets/68504324/81422217-b663-442b-bb10-b3354a9a5232)


The ping command never returns which means that the host is not responding to the ICMP packets of the ping command.  
This could be for different reasons:

- The host is blocking ICMP packets. The host may have a firewall or other security measure that prevents ICMP packets from reaching or leaving the host.
- The host is down or unreachable. The host may be powered off, disconnected from the network, or experiencing a hardware or software failure.
- The network is congested or faulty. There may be a problem with the routers, switches, cables, or other devices that connect you to the host. 
The network may be overloaded with traffic or experiencing interference.

However, the host may be accessible by other means, such as a web browser or a telnet client.


> Pinging Google:

![pinggoogle](https://github.com/jonfernq/Learning/assets/68504324/e7b56e51-bac3-4bcb-bab1-55f6081231db)

Once I issued a ping command, it continued to the ping the host over and over again without stopping, 
which at first was confusing. Howeever, I discovered that in Linux, the ping command by default sends 
packets to the host continuously until you press Ctrl+C to stop it. 
Also, the -c option can specify a number of packets to send and then stop. 

> Pinging 'localhost' my computer:

![pinglocalhost](https://github.com/jonfernq/Learning/assets/68504324/b58bbee4-eab3-4ddf-beb8-ab98321f8b32)


HOSTNAME 

> Comand: hostname

![hostname](https://github.com/jonfernq/Learning/assets/68504324/f1fc78ec-72fa-4e36-a040-74ff0bc0108d)

The command output shows the following information:

- The hostname command is used to display or set the hostname of the system, which is a name that identifies the system on a network.
- The output shows that the hostname of the system is DESKTOP-GBKF6GA, which is a string of characters that represents the system's name. This name may be assigned by the user, the administrator, or the operating system. It may also include a domain name or a suffix that indicates the network or organization that the system belongs to.


TRACEROUTE 

'Traceroute' traces the route that a packet takes to reach a destination host on a network. 
To find the path and the delay of each hop, packets are sent with increasing Time to Live (TTL) values, 
and the responses from intermediate routers are recorded.  The command is useful for:  

a. Troubleshooting network connectivity issues 
b. Identifying network bottlenecks 
c. Measuring network latency  

![traceroute1](https://github.com/jonfernq/Learning/assets/68504324/c1353bc6-29a5-45d3-9600-de372067e4b2)

Connecting from our machine to slashdot.org requires traversing 8 routers.


IP

The command 'ip a' is shorthand for 'ip address'.  This command shows or manipulates IP addresses 
and their properties. It can display information about all the network interfaces, 
including names, MAC addresses, IP addresses, subnet masks, broadcast addresses, and status (up or down). 
IP addresses and their properties on network interfaces can also be added, deleted, or modified.  

![ip_a](https://github.com/jonfernq/Learning/assets/68504324/c97550b9-dae9-4c3b-b3ac-b60f426ef420)


NETSTAT

The 'netstat' command is used to display network information or 'stats'.
This includes network connections, routing tables, interface statistics, masquerade connections, and multicast memberships. 
It is useful to: 

a. Monitor and troubleshoot network issues
b. Check status of network connections
c. Identify network bottlenecks
d. Find open ports.

> Command: netstat -ie

![netstat](https://github.com/jonfernq/Learning/assets/68504324/96ef4cea-a8ed-4ca1-8f0f-0cc3cd771693)


> Command: netstat -r

![netstat_routing_table](https://github.com/jonfernq/Learning/assets/68504324/097252a1-bc50-4271-9d20-5de5ab9a2357)


> Command: netstat -t 

![netstat_t](https://github.com/jonfernq/Learning/assets/68504324/5f949795-b4f2-4ebf-a558-1a76ff93d88b)



IFCONFIG

Ifconfig configures and displays info about network interfaces. 
It can show the name, flags, IP address, MAC address, and traffic details of network interfaces. 
It can change the IP address, subnet mask, broadcast address, and other properties of network interfaces.

> Command: /sbin/ifconfig 

![ifconfig](https://github.com/jonfernq/Learning/assets/68504324/af01d9cc-fa99-4922-a1a0-4d569f7cd186)



WHO & WHOAMI 


# Who & Whoami Commands

> I get no output for the command 'who' 
but I do get output for 'whoami' 
what is the meaning of this output? 

root@DESKTOP-GBKF6GA:/mnt/c/Users/ACER# who
root@DESKTOP-GBKF6GA:/mnt/c/Users/ACER# whoami
root

The meaning of this output is:

- If there are no users logged in to the system, the who command will produce no output.
The who command is used to display information about the users who are currently logged in to the system. 
It shows the user name, terminal name, login time, and remote host name for each user. 

- The whoami command is used to display the user name that is associated with the current process or shell. 
It does not depend on whether the user is logged in or not. In this case, the whoami command shows that the current 
effective user is root, which is the superuser or administrator of the system.


NMAP 

$ nmap <ip-address> 
$ nmap <server-ip-address>
  
Nmap scans networks and hosts for information including open ports, services, operating systems, vulnerabilities, etc.  

Command would only run in 'sudo' mode: 
  
> sudo nmap slashdot.org  
  
  ![nmap](https://github.com/jonfernq/Learning/assets/68504324/7d2f0fd7-7c49-469a-b2b5-b49e4e023b13)

  The output of the nmap command shows the following information:

- The version of nmap that is used and the website of nmap
- The date and time of the scan and the time zone
- The scan report for the target domain name (slashdot.org) and its IP address (104.18.29.86)
- The host status (up) and the latency (0.54s)
- The other IP address for slashdot.org that is not scanned (104.18.28.86)
- The number of ports that are not shown because they are filtered by a firewall or a router (996)
- The four ports that are open and their services (80 for http, 443 for https, 8080 for http-proxy, and 8443 for https-alt)

The output indicates that slashdot.org is online and has four web-related services running on different ports. It also indicates that most of the ports are filtered and not accessible by nmap.
  
  
TCPDUMP

'tcpdump' capture and analyzes network traffic. 
It display packets sent and received on a network interface, 
filtered based on protocol, source, destination, port, etc.


REFLECTIONS 

In exploring these commands, I generated a huge amount of output and explanation (approx 4000 words).
I had to sort through this to write-up this assignment.
What I discovered in the process is that all these commands potentially produce huge amounts of information.
Command options or piping into grep can be used to limit the information. 

I also found that these commands also potentially address the three central criteria of the discussion assignment: 
reliability, performance, and security. 

Performance can be inferred from the times returns by commands such as ping. 
Sometimes a host cannot be accessed. This might indicate host or network failure which has implications for reliability.
Or it could indicate that the host or network is configured to block the command, which has implications for security.
One perhaps use many commands in combination to infer reliability, performance, and security of a host or network. 



  
