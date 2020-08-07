# test - check file types and compare values

http://man7.org/linux/man-pages/man1/test.1.html

Bash File Testing

- `-b` filename - Block special file
- `-c` filename - Special character file
- `-d` directoryname - Check for directory Existence
- `-e` filename - Check for file existence, regardless of type (node, directory, socket, etc.)
- `-f` filename - Check for regular file existence not a directory
- `-G` filename - Check if file exists and is owned by effective group ID
- `-G` filename set-group-id - True if file exists and is set-group-id
- `-k` filename - Sticky bit
- `-L` filename - Symbolic link
- `-O` filename - True if file exists and is owned by the effective user id
- `-r` filename - Check if file is a readable
- `-S` filename - Check if file is socket
- `-s` filename - Check if file is nonzero size
- `-u` filename - Check if file set-user-id bit is set
- `-w` filename - Check if file is writable
- `-x` filename - Check if file is executable
