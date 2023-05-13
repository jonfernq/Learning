### IP COmmand

> What does the command 'ip a' do ?

The command 'ip a' is a shorthand for 'ip address', which is used to show or manipulate IP addresses and their properties on the system. It can display information about all the network interfaces, such as their names, MAC addresses, IP addresses, subnet masks, broadcast addresses, and status (up or down). It can also be used to add, delete, or modify IP addresses and their properties on the network interfaces.

For example, if you run 'ip a' on your system, you may see something like this:

```
1: lo: <LOOPBACK,UP,LOWER_UP> mtu 65536 qdisc noqueue state UNKNOWN group default qlen 1000
    link/loopback 00:00:00:00:00:00 brd 00:00:00:00:00:00
    inet 127.0.0.1/8 scope host lo
       valid_lft forever preferred_lft forever
    inet6 ::1/128 scope host 
       valid_lft forever preferred_lft forever
2: enp0s3: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc fq_codel state UP group default qlen 1000
    link/ether 08:00:27:a5:b2:c3 brd ff:ff:ff:ff:ff:ff
    inet 192.168.1.10/24 brd 192.168.1.255 scope global dynamic noprefixroute enp0s3
       valid_lft 86393sec preferred_lft 86393sec
    inet6 fe80::a00:27ff:fea5:b2c3/64 scope link noprefixroute 
       valid_lft forever preferred_lft forever
```

This output shows that there are two network interfaces on the system: lo and enp0s3. The lo interface is the loopback interface, which is used for internal communication within the system. It has an IPv4 address of 127.0.0.1 and an IPv6 address of ::1. The enp0s3 interface is the Ethernet interface, which is used for external communication with other devices on the network. It has an IPv4 address of 192.168.1.10 and an IPv6 address of fe80::a00:27ff:fea5:b2c3. It also shows the MAC address, the subnet mask, the broadcast address, and the state of each interface.

---

> Command: 'ip a'

**OUTPUT:**

![ip_a](https://github.com/jonfernq/Learning/assets/68504324/c97550b9-dae9-4c3b-b3ac-b60f426ef420)

---
The output of this 'ip a' command shows the following information:

- There are six network interfaces on the system: lo, bond0, dummy0, eth0, tunl0, and sit0.
- The lo interface is the loopback interface, which is used for internal communication within the system. It has an IPv4 address of 127.0.0.1 and an IPv6 address of ::1. It is in the UP state, which means it is active and ready to transmit and receive data.
- The bond0 interface is a bonding interface, which is used to combine multiple physical interfaces into one logical interface for redundancy or load balancing. It has a MAC address of 2a:7d:38:60:4d:53, but no IP address assigned. It is in the DOWN state, which means it is inactive and not ready to transmit or receive data.
- The dummy0 interface is a dummy interface, which is used to create virtual network interfaces for testing or simulation purposes. It has a MAC address of 7a:e0:d8:18:a7:7c, but no IP address assigned. It is also in the DOWN state.
- The eth0 interface is an Ethernet interface, which is used for external communication with other devices on the network. It has an IPv4 address of 172.23.48.85 and an IPv6 address of fe80::215:5dff:fe40:a0c4. It also has a subnet mask of /20 and a broadcast address of 172.23.63.255. It is in the UP state and uses the mq queueing discipline (qdisc) to manage multiple transmit queues.
- The tunl0 interface is a tunneling interface, which is used to encapsulate one protocol within another protocol for routing purposes. It uses the IP-in-IP protocol (ipip) to tunnel IP packets over IP networks. It has no MAC address or IP address assigned. It is in the DOWN state.
- The sit0 interface is another tunneling interface, which is used to tunnel IPv6 packets over IPv4 networks using the IPv6-in-IPv4 protocol (sit). It also has no MAC address or IP address assigned. It is also in the DOWN state.
