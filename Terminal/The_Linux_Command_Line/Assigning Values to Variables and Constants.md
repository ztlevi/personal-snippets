## Assigning Values to Variables and Constants

Here is where our knowledge of expansion really starts to pay off. As we have seen, variables are
assigned values this way:

```sh
variable=value
```

where variable is the name of the variable and value is a string. Unlike some other programming
languages, the shell does not care about the type of data assigned to a variable; it treats them all
as strings. You can force the shell to restrict the assignment to integers by using the `declare`
command with the `-i` option, but, like setting variables as read-only, this is rarely done. Note
that in an assignment, there must be no spaces between the variable name, the equal sign, and the
value. So what can the value consist of? Anything that we can expand into a string.

```sh
a=z                     # Assign the string "z" to variable a.
b="a string"            # Embedded spaces must be within ”" quotes.
c="a string and $b"     # Other expansions such as variables can be
                        # expanded into the assignment.
d=$(ls -l foo.txt)      # Results of a command.
e=$((5 * 7))           # Arithmetic expansion.
f="\t\ta string\n"     # Escape sequences such as tabs and newlines.”
```

Multiple variable assignments may be done on a single line:

```sh
a=5 b="a string"
```

During expansion, variable names may be surrounded by optional curly braces {}. This is useful in
cases where a variable name becomes ambiguous due to its surrounding context. Here, we try to change
the name of a file from myfile to myfile1, using a variable:

```sh
[me@linuxbox ˜]$ filename="myfile"
[me@linuxbox ˜]$ touch $filename
[me@linuxbox ˜]$ mv $filename $filename1
mv: missing destination file operand after `myfile'
Try `mv --help' for more information.
```

This attempt fails because the shell interprets the second argument of the mv command as a new (and
empty) variable. The problem can be overcome this way:

```sh
[me@linuxbox ˜]$ mv $filename ${filename}1
```

## Here Documents

We’ve looked at two different methods of outputting our text, both using the echo command. There is
a third way called a here document or here script. A here document is an additional form of I/O
redirection in which we embed a body of text into our script and feed it into the standard input of
a command. It works like this:

```sh
command<< token
text
token
```

Instead of using echo, our script now uses cat and a here document. The string `_EOF_` (meaning
end-of-file, a common convention) was selected as the token and marks the end of the embedded text.
Note that the token must appear alone and that there must not be trailing spaces on the line.

So what’s the advantage of using a here document? It’s mostly the same as echo, except that, by
default, single and double quotes within here documents lose their special meaning to the shell.
Here is a command-line example:

```sh
[me@linuxbox ˜]$ foo="some text”
[me@linuxbox ˜]$ cat << _EOF_
> $foo
> "$foo"
> '$foo'
> \$foo
> _EOF_
some text
"some text"
'some text'
$foo”
```

As we can see, the shell pays no attention to the quotation marks. It treats them as ordinary
characters. This allows us to embed quotes freely within a here document. This could turn out to be
handy for our report program.

Here documents can be used with any command that accepts standard input. In this example, we use a
here document to pass a series of commands to the ftp program in order to retrieve a file from a
remote FTP server:

```sh
#!/bin/bash

# Script to retrieve a file via FTP

FTP_SERVER=ftp.nl.debian.org
FTP_PATH=/debian/dists/lenny/main/installer-i386/current/images/cdrom
REMOTE_FILE=debian-cd_info.tar.gz

ftp -n << _EOF_
open $FTP_SERVER
user anonymous me@linuxbox
cd $FTP_PATH
hash
get $REMOTE_FILE
bye
_EOF_
ls -l $REMOTE_FILE
```

If we change the redirection operator from << to <<-, the shell will ignore leading tab characters
in the here document. This allows a here document to be indented, which can improve readability:

```sh
#!/bin/bash

# Script to retrieve a file via FTP

FTP_SERVER=ftp.nl.debian.org
FTP_PATH=/debian/dists/lenny/main/installer-i386/current/
images/cdromREMOTE_FILE=debian-cd_info.tar.gz

ftp -n <<- _EOF_
        open $FTP_SERVER
        user anonymous me@linuxbox
        cd $FTP_PATH
        hash
        get $REMOTE_FILE
        bye
        _EOF_

ls -l $REMOTE_FILE
```
