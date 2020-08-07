# UDP Multicast Setup

<!--header-->

Getting maximum performance on your LAN or local host

# Using LCM on a single host

Since LCM uses UDP Multicast as a transport mechanism, a valid multicast route must always be defined. This means that
to use LCM, even for inter-application communication on a single host, _you must have a multicast-enabled network
interface_. If your computer is already connected to the Internet, LCM will generally "just work" because the default
route will allow LCM to find the correct network interface for multicast traffic.

If your computer is not connected to any network, you may need to explicitly enable multicast traffic by adding
multicast entries to your system's routing table. On Linux, you can setup the loopback interface for multicast with the
following commands:

```sh
  sudo ifconfig lo multicast
  sudo route add -net 224.0.0.0 netmask 240.0.0.0 dev lo
```

Remember, you must always do this to use LCM if your machine is _not connected_ to any external network.
