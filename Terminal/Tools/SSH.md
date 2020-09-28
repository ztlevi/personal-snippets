# SSH

## SSH Port Forward

```
ssh -NfL 6006:localhost:6006 username@remote_server_address
# e.g.
ssh -NfL 6006:localhost:6006 root@10.213.37.34
```

## X11 Forwarding

> Note: Probly not need to do this in newer Ubuntu versions

https://unix.stackexchange.com/questions/12755/how-to-forward-x-over-ssh-to-run-graphics-applications-remotely

X11 forwarding needs to be enabled on both the client side and the server side.

On the **client side**, the `-X` (capital X) option to `ssh` enables X11 forwarding, and you can
make this the default (for all connections or for a specific conection) with `ForwardX11 yes` in
[`~/.ssh/config`](http://man.openbsd.org/OpenBSD-current/man5/ssh_config.5#ForwardX11).

On the **server side**, `X11Forwarding yes` must specified in
[`/etc/ssh/sshd_config`](http://man.openbsd.org/OpenBSD-current/man5/sshd_config.5#X11Forwarding).
Note that the default is no forwarding (some distributions turn it on in their default
`/etc/ssh/sshd_config`), and that the user cannot override this setting.

The `xauth` program must be installed on the server side. If there are any X11 programs there, it's
very likely that `xauth` will be there. In the unlikely case `xauth` was installed in a nonstandard
location, it can be called through
[`~/.ssh/rc`](http://man.openbsd.org/OpenBSD-current/man8/sshd.8#SSHRC) (on the server!).

Note that you do not need to set any environment variables on the server. `DISPLAY` and `XAUTHORITY`
will automatically be set to their proper values. If you run ssh and `DISPLAY` is not set, it means
ssh is not forwarding the X11 connection.

To confirm that ssh is forwarding X11, check for a line containing `Requesting X11 forwarding` in
the `ssh -v -X` output. Note that **the server won't reply** either way, a security precaution of
hiding details from potential attackers.

## Pycharm remote ssh handles external changes

`Build,Execution,Deployment` -> `Deployment` -> `Options` -> Uncheck `Skip external changes`

## Set up ssh keys

https://www.digitalocean.com/community/tutorials/how-to-set-up-ssh-keys--2

### About SSH Keys

Secure Shell (better known as SSH) is a cryptographic network protocol which allows users to
securely perform a number of network services over an unsecured network. SSH keys provide a more
secure way of logging into a server with SSH than using a password alone. While a password can
eventually be cracked with a brute force attack, SSH keys are nearly impossible to decipher by brute
force alone.

Generating a key pair provides you with two long string of characters: a public and a private key.
You can place the public key on any server, and then unlock it by connecting to it with a client
that already has the private key. When the two match up, the system unlocks without the need for a
password. You can increase security even more by protecting the private key with a passphrase.

Within some of the commands found in this tutorial, you will notice some highlighted values. These
are variables, and you should substitute them with your own values.

## Step One—Create the RSA Key Pair

The first step is to create the key pair on the client machine (there is a good chance that this
will just be your computer):

```
ssh-keygen -t rsa
```

## Step Two—Store the Keys and Passphrase

Once you have entered the Gen Key command, you will get a few more questions:

```
Enter file in which to save the key (/home/demo/.ssh/id_rsa):
```

You can press enter here, saving the file to the user home (in this case, my example user is called
demo).

```
Enter passphrase (empty for no passphrase):
```

It’s up to you whether you want to use a passphrase. Entering a passphrase does have its benefits:
the security of a key, no matter how encrypted, still depends on the fact that it is not visible to
anyone else. Should a passphrase-protected private key fall into an unauthorized users possession,
they will be unable to log in to its associated accounts until they figure out the passphrase,
buying the hacked user some extra time. The only downside, of course, to having a passphrase, is
then having to type it in each time you use the key pair.

The entire key generation process looks like this:

```
ssh-keygen -t rsa
```

```
OutputGenerating public/private rsa key pair.
Enter file in which to save the key (/home/demo/.ssh/id_rsa):
Enter passphrase (empty for no passphrase):
Enter same passphrase again:
Your identification has been saved in /home/demo/.ssh/id_rsa.
Your public key has been saved in /home/demo/.ssh/id_rsa.pub.
The key fingerprint is:
4a:dd:0a:c6:35:4e:3f:ed:27:38:8c:74:44:4d:93:67 demo@a
The key's randomart image is:
+--[ RSA 2048]----+
|          .oo.   |
|         .  o.E  |
|        + .  o   |
|     . = = .     |
|      = S = .    |
|     o + = +     |
|      . o + o .  |
|           . o   |
|                 |
+-----------------+
```

The public key is now located in `/home/demo/.ssh/id_rsa.pub`. The private key (identification) is
now located in `/home/demo/.ssh/id_rsa`.

## Step Three—Copy the Public Key

Once the key pair is generated, it’s time to place the public key on the server that we want to use.

You can copy the public key into the new machine’s authorized_keys file with the ssh-copy-id
command. Make sure to replace the example username and IP address below.

```
ssh-copy-id demo@198.51.100.0
```

**Note:** If you are a Mac user, ssh-copy-id will not be installed on your machine. You can,
however, install it using [Homebrew](https://brew.sh/):

```
brew install ssh-copy-id
```

Alternatively, you can paste in the keys using SSH:

```
cat ~/.ssh/id_rsa.pub | ssh demo@198.51.100.0 "mkdir -p ~/.ssh && chmod 700 ~/.ssh && cat >>  ~/.ssh/authorized_keys"
```

No matter which command you chose, you may see something like:

```
The authenticity of host '198.51.100.0 (198.51.100.0)' can't be established.
RSA key fingerprint is b1:2d:33:67:ce:35:4d:5f:f3:a8:cd:c0:c4:48:86:12.
Are you sure you want to continue connecting (yes/no)? yes
Warning: Permanently added '198.51.100.0' (RSA) to the list of known hosts.
user@198.51.100.0's password:
```

This message helps us to make sure that we haven’t added extra keys that you weren’t expecting.

Now you can go ahead and log into your user profile and you will not be prompted for a password.
However, if you set a passphrase when creating your SSH key, you will be asked to enter the
passphrase at that time (and whenever else you log in in the future).

## Optional Step Four—Disable the Password for Root Login

Once you have copied your SSH keys onto your server and **ensured that you can log in with the SSH
keys alone**, you can go ahead and restrict the root login to only be permitted via SSH keys.

In order to do this, open up the SSH config file:

```
sudo nano /etc/ssh/sshd_config
```

Within that file, find the line that includes `PermitRootLogin` and modify it to ensure that users
can only connect with their SSH key:

/etc/ssh/sshd_config

```
PermitRootLogin without-password
```

Save and close the file when you are finished.

To put these changes into effect:

```
sudo systemctl reload sshd.service
```

### DigitalOcean Addendum

The DigitalOcean control panel allows you to add public keys to your new Droplets when they’re
created. You can generate the SSH Key in a convenient location, such as the computer, and then
upload the public key to the SSH key section.

Then, when you create a new Droplet, you can choose to include that public key on the server. No
root password will be emailed to you and you can log in to your new server from your chosen client.
If you created a passphrase, you will be prompted to enter that upon login.
