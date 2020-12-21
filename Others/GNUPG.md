# GNUPG

https://www.youtube.com/watch?v=1vVIpIvboSg

Or use `gnome3.seahorse` to edit gpg keys

## Backup and Sync GPG

Mac's openssl location `/usr/local/Cellar/openssl@1.1/1.1.1h/bin/openssl`.

- Encrypt

```
tar cz gnupg | openssl enc -aes-256-cbc -pbkdf2 -e > gggpppggg.txt
```

- Decrypt

```
openssl enc -aes-256-cbc -pbkdf2 -d -in gggpppggg.txt | tar xz
```

Copy the files to `~/.gnupg` folder. Then `gpgconf --kill gpg-agent` to restart the agent.

## Common operations

```
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

### Export key

Option 1. Send it to public server

```
gpg --list-keys --keyid-format LONG ztlevi.work@gmail.com
>> pub   rsa4096/AAAAAAAAAAAAAAAA 2020-12-20 [SC]
>>       BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
>> uid                 [ultimate] Ting Zhou <ztlevi.work@gmail.com>
>> sub   rsa4096/CCCCCCCCCCCCCCCC 2020-12-20 [E]

gpg --send-keys AAAAAAAAAAAAAAAA
>>> gpg: sending key AAAAAAAAAAAAAAAA to hkps://keys.openpgp.org
```

Option 2. Generate and output the public to a file.

```
gpg --export --armor --output ztlevi.gpg.pub ztlevi.work@gmail.com
```

### Import key

```
gpg --import ztlevi.gpg.pub
```
