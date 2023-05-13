## Netstat Command

> What is the 'netstat' command used for ?

The 'netstat' command is used to display various network-related information, such as network connections, routing tables, interface statistics, masquerade connections, and multicast memberships. It can help to monitor and troubleshoot network issues, such as checking the status of network connections, identifying network bottlenecks, and finding open ports.

The 'netstat' command has many options and switches that can modify its output and behavior. For example, some common options are:

- -a: show all sockets (including listening sockets)
- -n: show numerical addresses (instead of resolving hostnames)
- -t: show TCP connections only
- -u: show UDP connections only
- -r: show the routing table
- -i: show the interface statistics
- -s: show the protocol statistics

For example, if you run 'netstat -an', you may see something like this:

```
Active Internet connections (servers and established)
Proto Recv-Q Send-Q Local Address           Foreign Address         State
tcp        0      0 0.0.0.0:22              0.0.0.0:*               LISTEN
tcp        0      0 192.168.1.10:22         192.168.1.5:54321       ESTABLISHED
tcp6       0      0 :::80                   :::*                    LISTEN
udp        0      0 127.0.0.53:53           0.0.0.0:*
udp        0      0 192.168.1.10:68         0.0.0.0:*
udp6       0      0 fe80::215:5dff:fe40::546 :::*
```

This output shows that there are four TCP sockets and three UDP sockets on the system. The first column shows the protocol, the second and third columns show the receive and send queues, the fourth and fifth columns show the local and foreign addresses and ports, and the sixth column shows the state of the connection.

For TCP sockets, the state can be one of the following:

- LISTEN: waiting for incoming connections
- ESTABLISHED: connection established
- SYN_SENT: actively trying to establish connection
- SYN_RECV: initial packet received from peer
- FIN_WAIT1: connection closed locally, waiting for remote acknowledgement
- FIN_WAIT2: remote end has shut down, waiting for socket to close
- TIME_WAIT: socket closed, waiting for enough time to pass to be sure remote end received acknowledgement
- CLOSE_WAIT: remote end has shut down, waiting for socket to close locally
- LAST_ACK: remote end has shut down and socket is closed locally, waiting for acknowledgement
- CLOSE: socket is not being used

For UDP sockets, there is no state because UDP is a connectionless protocol.

---
> Installing 'netstat'

Netstat is not a separate package, but a command that is part of the net-tools package. 
Therefore, to install netstat, you need to install net-tools instead. 
You can use the following command:

`sudo apt-get install net-tools`

This will install netstat and other network-related commands, such as ifconfig, route, arp, etc.

---
> Command: netstat -ie

**OUTPUT:**

![netstat](https://github.com/jonfernq/Learning/assets/68504324/96ef4cea-a8ed-4ca1-8f0f-0cc3cd771693)

The output of this netstat command shows the following information:

- The -i option is used to display the interface statistics, such as the name, flags, IP address, MAC address, and traffic details of each network interface on the system.
- There are two network interfaces on the system: eth0 and lo.
- The eth0 interface is an Ethernet interface, which is used for external communication with other devices on the network. It has an IPv4 address of 172.23.48.85 and an IPv6 address of fe80::215:5dff:fe40:a0c4. It also has a subnet mask of 255.255.240.0 and a broadcast address of 172.23.63.255. It has a MAC address of 00:15:5d:40:a0:c4 and a maximum transmission unit (MTU) of 1500 bytes. It has several flags that indicate its status, such as UP (active), BROADCAST (supports broadcasting), RUNNING (ready to transmit and receive data), and MULTICAST (supports multicasting). It has received 2370 packets and 606933 bytes, and transmitted 506 packets and 155923 bytes, without any errors, drops, overruns, frame errors, carrier errors, or collisions.
- The lo interface is the loopback interface, which is used for internal communication within the system. It has an IPv4 address of 127.0.0.1 and an IPv6 address of ::1. It has a subnet mask of 255.0.0.0 and no broadcast address. It has no MAC address and an MTU of 65536 bytes. It has three flags that indicate its status: UP (active), LOOPBACK (loopback device), and RUNNING (ready to transmit and receive data). It has received and transmitted 50 packets and 4200 bytes each, without any errors, drops, overruns, frame errors, carrier errors, or collisions.

---
> Command: netstat -r

![netstat_routing_table](https://github.com/jonfernq/Learning/assets/68504324/097252a1-bc50-4271-9d20-5de5ab9a2357)

The output of this netstat command shows the following information:

- The -r option is used to display the routing table, which is a set of rules that determine where to send packets based on their destination address.
- There are two entries in the routing table: default and 172.23.48.0.
- The default entry is the default route, which is used when no other route matches the destination address. It has a gateway of DESKTOP-GBKF6GA, which is the IP address or hostname of the router that connects to the internet or other networks. It has a genmask of 0.0.0.0, which means that any destination address can match this route. It has a flag of UG, which means that the route is up (active) and uses a gateway. It has an interface of eth0, which is the Ethernet interface that connects to the router.
- The 172.23.48.0 entry is a network route, which is used to send packets to a specific network or subnet. It has no gateway, which means that the packets are sent directly to the destination without passing through a router. It has a genmask of 255.255.240.0, which means that only the destination addresses that start with 172.23.48.x to 172.23.63.x can match this route. It has a flag of U, which means that the route is up (active). It also has an interface of eth0, which is the Ethernet interface that connects to the network or subnet.

---
> Please explain the options on the following commands: 
netstat –t
netstat –s –t
netstat -i

The options on the following commands are:

- netstat –t: This command displays the network connections for the TCP protocol only. The TCP protocol is a connection-oriented protocol that provides reliable and ordered delivery of data between two endpoints. By default, netstat shows both TCP and UDP connections, but the -t option filters out the UDP connections.
- netstat –s –t: This command displays the protocol statistics for the TCP protocol only. The protocol statistics show the number of packets and bytes sent and received for each protocol, as well as the errors and drops that occurred. The -s option enables the statistics mode, and the -t option filters out the other protocols except TCP.
- netstat -i: This command displays the interface statistics for all network interfaces on the system. The interface statistics show the name, flags, IP address, MAC address, and traffic details of each network interface. The -i option enables the interface mode.

---
> Command: netstat -t 

![netstat_t](https://github.com/jonfernq/Learning/assets/68504324/5f949795-b4f2-4ebf-a558-1a76ff93d88b)

The output of this netstat command shows the following information:

- The -t option is used to display the network connections for the TCP protocol only. However, there are no TCP connections on the system, so the first part of the output is empty.
- The second part of the output shows the active UNIX domain sockets on the system. UNIX domain sockets are a type of inter-process communication (IPC) that allow processes on the same system to exchange data using a file-like interface. They are identified by a path name instead of an IP address and port number.
- Each line of the output shows a different UNIX domain socket and its properties. The first column shows the protocol, which is always UNIX for UNIX domain sockets. The second column shows the reference count, which is the number of file descriptors that refer to the socket. The third column shows the flags, which indicate the state and type of the socket. The fourth column shows the type, which can be one of STREAM (reliable byte-stream), DGRAM (unreliable datagram), SEQPACKET (reliable datagram), or RAW (raw data). The fifth column shows the state, which can be one of FREE (not allocated), LISTENING (waiting for incoming connections), CONNECTING (actively trying to establish connection), CONNECTED (connection established), DISCONNECTING (connection closing), or UNKNOWN (unknown state). The sixth column shows the inode number, which is a unique identifier for the socket file. The seventh column shows the path name, which is the name of the socket file in the file system.
- For example, the first line of the output shows a UNIX domain socket with a reference count of 3, no flags, a type of STREAM, a state of CONNECTED, an inode number of 19485, and no path name. This means that there are three file descriptors that refer to this socket, it is a reliable byte-stream socket, it has an established connection with another process on the system, and it has no name in the file system.

---











