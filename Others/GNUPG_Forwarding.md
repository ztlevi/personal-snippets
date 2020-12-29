[AgentForwarding - GnuPG wiki](https://wiki.gnupg.org/AgentForwarding)

https://gist.github.com/TimJDFletcher/85fafd023c81aabfad57454111c1564d

## Forwarding gpg-agent to a remote system over SSH

[GnuPG](https://wiki.gnupg.org/GnuPG) 2.1 enables you to forward the
[GnuPG](https://wiki.gnupg.org/GnuPG)\-Agent to a remote system. That means that you can keep your
secret keys on a local machine (or even a hardware token like a smartcard or on a
[GNUK](https://wiki.gnupg.org/GNUK)).

You need at least [GnuPG](https://wiki.gnupg.org/GnuPG) 2.1.1 on both systems.

### GnuPG configuration

For really old versions of [GnuPG](https://wiki.gnupg.org/GnuPG) (< 2.1.17) you need to edit your
gpg-agent.conf to configure an extra socket. The extra socket is more restricted then the normal
socket and Pinentry messages will differ when gpg-agent is accessed over this socket:

extra-socket /home/<user>/.gnupg/S.gpg-agent.extra

Replace <user> with your actual username. Current versions of [GnuPG](https://wiki.gnupg.org/GnuPG)
create the socket by default.

#### GnuPG on the remote system

It is important to note that to work properly [GnuPG](https://wiki.gnupg.org/GnuPG) on the remote
system still needs your public keys. So you have to make sure they are available on the remote
system even if your secret keys are not.

### SSH Configuration

**Note:** With [GnuPG](https://wiki.gnupg.org/GnuPG) >= 2.1.13 the location of the agents socket
changed. To find out the name of the local socket, always use:

```sh
gpgconf --list-dir agent-extra-socket
```

this is the name of the extra socket on the local box. On the server the standard socket needs to be
configured. Find this one out with

```sh
gpgconf --list-dir agent-socket
```

#### OpenSSH >= 6.7

To your /.ssh/config you can add:

```
Host gpgtunnel
HostName server.domain
RemoteForward <socket_on_remote_box> <extra_socket_on_local_box>
```

If you can modify the servers settings you should put:

```
StreamLocalBindUnlink yes
```

Into /etc/ssh/sshd_config to enable automatic removal of stale sockets when connecting to the remote
machine. Otherwise you will first have to remove the socket on the remote machine before forwarding
works.

With [StreamLocalBindUnlink](https://wiki.gnupg.org/StreamLocalBindUnlink) yes you can connect with
ssh gpgtunnel and just use [GnuPG](https://wiki.gnupg.org/GnuPG) as usual.

**Note:** On Systems where systemd controls the directories under /var/run/user/<uid> it may be that
the socket forwarding fails because /var/run/user/<uid>/gnupg is deleted on logout. To workaround
this you can put gpgconf --create-socketdir in the startup script of your shell e.g. ~/.bashrc or
~/.zshrc.

Remote gpg will try to start gpg-agent if it's not running. Remote gpg-agent which will delete your
forwarded socket and set up it's own. To avoid this you can pass \--no-autostart to remote gpg
command.
