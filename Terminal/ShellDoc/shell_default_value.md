# Default value

## `${parameter:-[word]}`

    Use Default Values. If parameter is unset or null, the expansion of word (or an empty string if word is omitted) shall be substituted; otherwise, the value of parameter shall be substituted.

## `${parameter:=[word]}`

Assign Default Values. If parameter is unset or null, the expansion of word (or an empty string if word is omitted)
shall be assigned to parameter. In all cases, the final value of parameter shall be substituted. Only variables, not
positional parameters or special parameters, can be assigned in this way.

## `${parameter:?[word]}`

Indicate Error if Null or Unset. If parameter is unset or null, the expansion of word (or a message indicating it is
unset if word is omitted) shall be written to standard error and the shell exits with a non-zero exit status. Otherwise,
the value of parameter shall be substituted. An interactive shell need not exit.

## `${parameter:+[word]}`

Use Alternative Value. If parameter is unset or null, null shall be substituted; otherwise, the expansion of word (or an
empty string if word is omitted) shall be substituted.
