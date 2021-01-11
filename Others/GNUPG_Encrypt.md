[How to create compressed encrypted archives with tar and gpg - LinuxConfig.org](https://linuxconfig.org/how-to-create-compressed-encrypted-archives-with-tar-and-gpg)

## Create an Encrypted Archive

Now that we have reviewed creating an archive with `tar`, let’s look at how we can create an
encrypted archive by adding `gpg` to the mix. You may choose to use key based encryption, password
based encryption or a combination of both. We have already looked at using key based encryption in a
article
[How to Encrypt and Decrypt Individual Files With GPG](https://linuxconfig.org/how-to-encrypt-and-decrypt-individual-files-with-gpg),
so we will look at password based encryption here. To create an encrypted compressed archive of a
directory named folder enter the following command.

```sh
# encrypt with symmetric passphrase
tar -cvzf - folder | gpg -c > folder.tar.gz.gpg
# encrypt with GPG key
tar -cvzf - folder | gpg --encrypt -r ztlevi.work@gmail.com > folder.tar.gz.gpg
```

All of the `tar` flags are the same as in our previous example. The only difference is that instead
of specifying a filename for our archive within the tar command we specify `-` so that we can pipe
the output of the `tar` command into `gpg`. We then proceed to do just that and `gpg`‘s `-c` flag
indicates that we want to encrypt the file with a symmetric cipher using a passphrase as we
indicated above. Finally, we redirect the output to a file named `folder.tar.gz.gpg` with `>`. After
entering this command you will be prompted to enter the passphrase that you want to use to encrypt
the data. If you don’t like this behavior and prefer to specify the passphrase within the command
you may add the `--passphrase` flag after `-c` as shown below.

**WARNING**  
Specifying a passphrase on the command line using the --passphrase is less secure for multiple
reasons. It will save the password in your bash history (or any other shell history file). Also, if
you are on a multi-user system then other users may see your password be examining running
processes. Even if you are the single user of a system, any software that is capable of examining
currently running processes could potentially log your passphrase.

```sh
tar -cvzf - folder | gpg -c --passphrase yourpassword > folder.tar.gz.gpg
```

## Decrypt

In order to decrypt, decompress and extract this archive later you would enter the following
command.

```sh
gpg -d folder.tar.gz.gpg | tar -xvzf -
```

The `-d` flag tells `gpg` that we want to decrypt the contents of the `folder.tar.gz.gpg` file. We
then pipe that to the tar command. The `-x` flag is used to extract the archive that is piped in
from gpg, `-v` is for verbose extracting, `-z` is to decompress the archive and `-f -` specifies
that the file being unarchived is being piped in.
