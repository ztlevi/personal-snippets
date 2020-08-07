# IP Route

https://www.cyberciti.biz/tips/configuring-static-routes-in-debian-or-red-hat-linux-systems.html

https://www.cyberciti.biz/faq/linux-setup-default-gateway-with-route-command/

```
route
route -n
ip route list
```

## Use wifi as default and add specific IP address's gateway

enp3s0 is the ethernet device name

```
# Delete default ethernet cable net
sudo route del default gw 0.0.0.0 enp3s0

# Route specific IP to gateway IP, connecting using ethernet
sudo ip route add 10.213.37.0/24 via 10.193.35.1 dev enp3s0
```

> 10.0.0.0/24 is 10.0.0.1-10.0.0.255

> 10.0.0.0/8 is 10.0.0.0-10.255.255.255

> subnet = 192.168.1.0 = 192.168.1.1 to .254

> mask = /24 = 255.255.255.0
