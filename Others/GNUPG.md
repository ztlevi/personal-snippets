# GNUPG

https://www.youtube.com/watch?v=1vVIpIvboSg

Or use `gnome3.seahorse` to edit gpg keys

## Backup and Sync GPG

### [Export and import private key](https://access.redhat.com/solutions/2115511)

```sh
# Mac openssl location
alias openssl=/usr/local/Cellar/openssl@1.1/1.1.1h/bin/openssl

# Export
mkdir -p ~/Downloads/mygpgexport
cd ~/Downloads/mygpgexport
gpg -a --export >mypubkeys.asc
gpg -a --export-secret-keys >myprivatekeys.asc
gpg --export-ownertrust >otrust.txt

# Import
cd ~/Downloads/mygpgexport
gpg --import myprivatekeys.asc
gpg --import mypubkeys.asc
gpg -K
gpg -k
gpg --import-ownertrust otrust.txt

# restart agent
gpgconf --kill gpg-agent
```

### (Optional) export specific key

```sh
gpg --list-secret-keys ztlevi.work@gmail.com
gpg --export-secret-keys ztlevi.work@gmail.com > ztlevi-secret.key
gpg --import ztlevi-secret.key
```

## Common operations

```sh
gpg --list-keys
gpg --full-generate-key

gpg --edit-key ztlevi.work@gmail.com
gpg> list
gpg> key 0
gpg> expire # change expiration date
# also change key 1, another sub key
gpg> save
```

## Revoke

```
gpg --output revoke-ztlevi.asc --gen-revoke ztlevi.work@gmail.com
```

## Public key

```
gpg --export --armor ztlevi.work@gmail.com
gpg --export --armor --output ztlevi.gpg.pub ztlevi.work@gmail.com
```

Add this gpg public key to Github->settings->SSH and GPG keys.

## Share public key

### Export public key

Option 1. Send it to public server

```
gpg --list-keys --keyid-format LONG ztlevi.work@gmail.com
 pub   rsa4096/AAAAAAAAAAAAAAAA 2020-12-20 [SC]
       BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
 uid                 [ultimate] Ting Zhou <ztlevi.work@gmail.com>
 sub   rsa4096/CCCCCCCCCCCCCCCC 2020-12-20 [E]

gpg --send-keys AAAAAAAAAAAAAAAA
 gpg: sending key AAAAAAAAAAAAAAAA to hkps://keys.openpgp.org
```

Option 2. Generate and output the public to a file.

```
gpg --export --armor --output ztlevi.gpg.pub ztlevi.work@gmail.com
```

### Import public key

```
gpg --import ztlevi.gpg.pub
```
