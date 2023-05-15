## Nmap Command

> Please explain the nmap command below and why it can take two arguments: ip-address & server-ip-address ? 

$ nmap <ip-address> 
$ nmap <server-ip-address>
  
The nmap command is used to scan networks and hosts for various information, such as open ports, services, operating systems, vulnerabilities, etc. It can perform different types of scans, such as TCP, UDP, SYN, ACK, etc. It can also use various options and switches to modify its output and behavior.

The nmap command can take one or more arguments that specify the target(s) to scan. The arguments can be IP addresses, hostnames, domain names, network ranges, etc. For example:

- nmap 192.168.1.10: This command scans the host with the IP address 192.168.1.10 for open ports and services.
- nmap example.com: This command scans the host with the domain name example.com for open ports and services. It first resolves the domain name to an IP address using DNS and then scans that IP address.
- nmap 192.168.1.0/24: This command scans the network range 192.168.1.0 to 192.168.1.255 for open ports and services on each host in the range.
- nmap 192.168.1.10 example.com: This command scans both the host with the IP address 192.168.1.10 and the host with the domain name example.com for open ports and services.

Therefore, the nmap command below can take two arguments: ip-address and server-ip-address because they are both valid targets to scan. The ip-address argument can be any IP address of a host or a network range to scan, and the server-ip-address argument can be any IP address of a server or a domain name of a server to scan.
  
---
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
  
  
  
  
  
