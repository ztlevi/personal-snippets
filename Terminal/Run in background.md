## & After a Command with /dev/null

Adding `&` after a command will push a command into the background, but as a result the background
command will continue to print messages into the terminal as you’re using it. If you’re looking to
prevent this, consider redirecting the command to `/dev/null`.

```
command &>/dev/null &
```

This does not prevent the command from closing when the terminal closes. However, like mentioned
above, it’s possible to use `disown` to disown the running command away from the user.

## Nohup, with & and /dev/null

Unlike the previous commands, using `nohup` allows you to run a command in the background and keep
it running. How? Nohup bypasses the HUP signal (signal hang up), making it possible to run commands
in the background even when the terminal is off. Combine this command with redirection to
`/dev/null` (to prevent nohup from making a nohup.out file), and everything goes to the background
with one command.

```
nohup command &>/dev/null &
```
