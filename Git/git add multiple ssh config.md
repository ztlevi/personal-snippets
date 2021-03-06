# https://stackoverflow.com/questions/3225862/multiple-github-accounts-ssh-config

## Step 1: ssh keys

Create any keypairs you'll need. In this example I've named me default/original 'id_rsa' (which is
the default) and my new one 'id_rsa-work':

```
ssh-keygen -t rsa -C "stefano@work.com"
```

## Step 2: ssh config

Set up multiple ssh profiles by creating/modifying ~/.ssh/config. Note the slightly differing 'Host'
values:

```
# Default GitHub
Host github.com
    HostName github.com
    PreferredAuthentications publickey
    IdentityFile ~/.ssh/id_rsa

# Work GitHub
Host work.github.com
    HostName github.com
    PreferredAuthentications publickey
    IdentityFile ~/.ssh/id_rsa_work
```

## Step 3: ssh-add

You may or may not have to do this. To check, list identity fingerprints by running:

```
$ ssh-add -l
2048 1f:1a:b8:69:cd:e3:ee:68:e1:c4:da:d8:96:7c:d0:6f stefano (RSA)
2048 6d:65:b9:3b:ff:9c:5a:54:1c:2f:6a:f7:44:03:84:3f stefano@work.com (RSA)
```

If your entries aren't there then run:

```
ssh-add ~/.ssh/id_rsa_work
```

## Step 4: test

To test you've done this all correctly, I suggest the following quick check:

```
$ ssh -T git@github.com
```

Hi stefano! You've successfully authenticated, but GitHub does not provide shell access.

```
$ ssh -T git@work.github.com
```

Hi stefano! You've successfully authenticated, but GitHub does not provide shell access. Note that
you'll have to change the hostname (github / work.github) depending on what key/identity you'd like
to use. But now you should be good to go! :)
