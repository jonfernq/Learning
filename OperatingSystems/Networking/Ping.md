## Ping Command

> For what purpose are 'ping' commands issued?

A ping command is issued for the purpose of testing the connectivity and performance of a host on a network. A ping command works by sending ICMP packets to the host and listening for ICMP replies. The ping command shows you how long each packet took to make the round trip or tells you there was no reply. A ping command can help you to:

- Discover if a host is up and running.
- Check to see if a webserver is up and running (TCP ping).
- Network performance analysis.
- Troubleshoot network problems.
- Check if the elements of the network  (its interface cards, cabling, routing, and gateways) are working properly and how fast they are responding.

ICMP packets are used by the ping command to test the connectivity and performance of a host on a network. Most network devices will reply to these packets by default, unless they are configured otherwise. Some network devices may ignore or block ICMP packets for security reasons, such as hiding their presence or preventing denial-of-service attacks. Firewalls may also filter ICMP packets to protect the network from unwanted traffic.

---
> What is this: 201.54.100.1 ?

This is an IP address, which is a numerical label assigned to each device connected to a computer network that uses the Internet Protocol for communication¹. 
An IP address serves two main functions: host or network interface identification and location addressing¹.

The IP address 201.54.100.1 belongs to the **Brazil** country code and the **Sao Paulo** region². 
It is associated with the **Telesp** internet service provider².

You can use the ping command to test whether this IP address is reachable across an IP network. 
Ping works by sending ICMP packets to the host and listening for ICMP replies³. 
The ping command shows you how long each packet took to make the round trip or tells you there was no reply³. 
To use ping, you can open a terminal or command prompt window and type "ping 201.54.100.1" and hit enter³.

Source: Conversation with Bing, 5/13/2023
(1) How to Use the Ping Command to Test Your Network - How-To Geek. https://www.howtogeek.com/355664/how-to-use-ping-to-test-your-network/.
(2) Ping IP to test whether a particular host is reachable ... - DNSlytics. https://dnslytics.com/ping.
(3) 13 Linux Network Configuration and Troubleshooting Commands - Tecmint. https://www.tecmint.com/linux-network-configuration-and-troubleshooting-commands/.

---
> Pinging: 201.54.100.1 

![100percentloss](https://github.com/jonfernq/Learning/assets/68504324/81422217-b663-442b-bb10-b3354a9a5232)

> What does it mean if the ping command never returns?

If the ping command never returns, it means that the host you are trying to reach is not responding to your ICMP packets. This could happen for several reasons, such as:

- The host is down or unreachable. The host may be powered off, disconnected from the network, or experiencing a hardware or software failure.
- The host is blocking ICMP packets. The host may have a firewall or other security measure that prevents ICMP packets from reaching or leaving the host.
- The network is congested or faulty. There may be a problem with the routers, switches, cables, or other devices that connect you to the host. The network may be overloaded with traffic or experiencing interference.
- The ping command is interrupted or timed out. You may have pressed Ctrl+C to stop the ping command, or the ping command may have reached its maximum number of attempts or duration without getting a reply.

To troubleshoot this issue, you can try the following steps:

- Check if the host is online and accessible by other means, such as a web browser or a telnet client.
- Check if the host has a firewall or other security measure that blocks ICMP packets, and if possible, disable it temporarily or allow ICMP packets through it.
- Check if the network is working properly by pinging other hosts on the same network or on different networks. You can also use traceroute or pathping commands to see the route and performance of your packets to the host.
- Check if the ping command is configured correctly and has enough time and attempts to get a reply. You can use different options with the ping command to adjust its behavior, such as -t to ping continuously until stopped, -w to set the timeout for each reply, or -n to set the number of packets to send.

---

> How do you get the IP address of Google ?

One way to get the IP address of Google is to use the ping command with the URL www.google.com. Ping works by sending ICMP packets to the host and listening for ICMP replies¹. The ping command shows you the IP address associated with the URL on the first line of the output¹. To use ping, you can open a terminal or command prompt window and type "ping www.google.com" and hit enter¹.

Another way to get the IP address of Google is to use Google Public DNS, which is a free, global Domain Name System (DNS) resolution service that you can use as an alternative to your current DNS provider². Google Public DNS maintains the IP addresses 8.8.8.8 and 8.8.4.4 as the primary and secondary DNS addresses for Google Public DNS². A network of DNS servers strategically located around the world support queries at these addresses². To use Google Public DNS, you need to explicitly change the DNS settings in your operating system or device to use the Google Public DNS IP addresses².

Source: Conversation with Bing, 5/13/2023
(1) The IP Addresses Used by Google - Lifewire. https://www.lifewire.com/what-is-the-ip-address-of-google-818153.
(2) Get Started | Public DNS | Google Developers. https://developers.google.com/speed/public-dns/docs/using.
(3) Google IP - IP Address Info. https://ipinfo.info/google/.

---
> Pinging Google:

![pinggoogle](https://github.com/jonfernq/Learning/assets/68504324/e7b56e51-bac3-4bcb-bab1-55f6081231db)

---
> Pinging 'localhost' my computer:

![pinglocalhost](https://github.com/jonfernq/Learning/assets/68504324/b58bbee4-eab3-4ddf-beb8-ab98321f8b32)

---
> Comand: hostname

![hostname](https://github.com/jonfernq/Learning/assets/68504324/f1fc78ec-72fa-4e36-a040-74ff0bc0108d)

The command output shows the following information:

- The hostname command is used to display or set the hostname of the system, which is a name that identifies the system on a network.
- The output shows that the hostname of the system is DESKTOP-GBKF6GA, which is a string of characters that represents the system's name. This name may be assigned by the user, the administrator, or the operating system. It may also include a domain name or a suffix that indicates the network or organization that the system belongs to.
- 
---
> Once you issue a ping command, why does it continue to the ping the host over and over again?

The ping command may continue to ping the host over and over again for different reasons, depending on the operating system and the options used with the ping command. For example:

- In Windows, the ping command by default sends four packets to the host and then stops. However, if you use the -t option with the ping command, it will ping the host continuously until you press Ctrl+C to stop it. This option can be useful for monitoring the availability and performance of the host over time.
- In Linux, macOS, and other Unix-like systems, the ping command by default sends packets to the host continuously until you press Ctrl+C to stop it. However, you can use the -c option with the ping command to specify the number of packets to send and then stop. This option can be useful for testing the connectivity and response time of the host in a single run.















