## Ifconfig Command

> Please explain the 'ifconfig' command

The ifconfig command is used to configure and display information about the network interfaces on the system. It can show the name, flags, IP address, MAC address, and traffic details of each network interface. It can also be used to change the IP address, subnet mask, broadcast address, and other properties of the network interfaces.

The ifconfig command has many options and switches that can modify its output and behavior. For example, some common options are:

- -a: show all interfaces (including inactive ones)
- -s: show a summary of the interface statistics
- -v: show verbose output
- interface: show or configure a specific interface (such as eth0 or lo)
- up: activate an interface
- down: deactivate an interface

For example, if you run 'ifconfig -a', you may see something like this:

```
eth0: flags=4163<UP,BROADCAST,RUNNING,MULTICAST>  mtu 1500
        inet 172.23.48.85  netmask 255.255.240.0  broadcast 172.23.63.255
        inet6 fe80::215:5dff:fe40:a0c4  prefixlen 64  scopeid 0x20<link>
        ether 00:15:5d:40:a0:c4  txqueuelen 1000  (Ethernet)
        RX packets 2370  bytes 606933 (606.9 KB)
        RX errors 0  dropped 0  overruns 0  frame 0
        TX packets 506  bytes 155923 (155.9 KB)
        TX errors 0  dropped 0 overruns 0  carrier 0  collisions 0

lo: flags=73<UP,LOOPBACK,RUNNING>  mtu 65536
        inet 127.0.0.1  netmask 255.0.0.0
        inet6 ::1  prefixlen 128  scopeid 0x10<host>
        loop  txqueuelen 1000  (Local Loopback)
        RX packets 50  bytes 4200 (4.2 KB)
        RX errors 0  dropped 0  overruns 0  frame 0
        TX packets 50  bytes 4200 (4.2 KB)
        TX errors 0 dropped overruns carrier collisions

sit0: flags=128<NOARP> mtu1480
        sit   txqueuelen1000 (IPv6-in-IPv4)
```

This output shows that there are three network interfaces on the system: eth0, lo, and sit0. The eth0 interface is an Ethernet interface, which is used for external communication with other devices on the network. It has an IPv4 address of172.23.48.85 and an IPv6 address of fe80::215:5dff:fe40:a0c4. It also has a subnet mask of255.255.240.0 and a broadcast address of172.23.63.255 . It has a MAC address of00:15:5d:40:a0:c4 and a maximum transmission unit (MTU) of1500 bytes . It has several flags that indicate its status, such as UP (active), BROADCAST (supports broadcasting), RUNNING (ready to transmit and receive data), and MULTICAST (supports multicasting). It has received2370 packets and606933 bytes , and transmitted506 packets and155923 bytes , without any errors, drops, overruns, frame errors, carrier errors, or collisions.

The lo interface is the loopback interface, which is used for internal communication within the system. It has an IPv4 address of127.0.0.1 and an IPv6 address of ::1 . It has a subnet mask of255 . It has no MAC address and an MTU of65536 bytes . It has three flags that indicate its status: UP (active), LOOPBACK (loopback device), and RUNNING (ready to transmit and receive data). It has received and transmitted50 packets and4200 bytes each , without any errors, drops, overruns, frame errors, carrier errors, or collisions.

The sit0 interface is a tunneling interface, which is used to tunnel IPv6 packets over IPv4 networks using the IPv6-in-IPv4 protocol (sit). It has no IP address or MAC address assigned . It has an MTU of1480 bytes . It has one flag that indicates its status: NOARP (no ARP protocol). It has no traffic details.

---
> Command: /sbin/ifconfig 

![ifconfig](https://github.com/jonfernq/Learning/assets/68504324/af01d9cc-fa99-4922-a1a0-4d569f7cd186)


The output of this ifconfig command shows the following information:

- The /sbin/ifconfig command is used to display information about the network interfaces on the system. It shows the name, flags, IP address, MAC address, and traffic details of each network interface.
- There are two network interfaces on the system: eth0 and lo.
- The eth0 interface is an Ethernet interface, which is used for external communication with other devices on the network. It has an IPv4 address of 172.23.48.85 and an IPv6 address of fe80::215:5dff:fe40:a0c4. It also has a subnet mask of 255.255.240.0 and a broadcast address of 172.23.63.255. It has a MAC address of 00:15:5d:40:a0:c4 and a maximum transmission unit (MTU) of 1500 bytes. It has several flags that indicate its status, such as UP (active), BROADCAST (supports broadcasting), RUNNING (ready to transmit and receive data), and MULTICAST (supports multicasting). It has received 2488 packets and 631689 bytes, and transmitted 510 packets and 156161 bytes, without any errors, drops, overruns, frame errors, carrier errors, or collisions.
- The lo interface is the loopback interface, which is used for internal communication within the system. It has an IPv4 address of 127.0.0.1 and an IPv6 address of ::1. It has a subnet mask of 255.0.0.0 and no broadcast address. It has no MAC address and an MTU of 65536 bytes. It has three flags that indicate its status: UP (active), LOOPBACK (loopback device), and RUNNING (ready to transmit and receive data). It has received and transmitted 50 packets and 4200 bytes each, without any errors, drops, overruns, frame errors, carrier errors, or collisions.



