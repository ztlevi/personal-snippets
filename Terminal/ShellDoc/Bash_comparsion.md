# Bash operators `[[` vs `[` vs `(` vs `((`

The `then` clause is executed if the exit code of `commands1` is zero. If the exit code is nonzero,
then the `else` clause is executed. `commands1` can be simple or complex. It can, for example, be a
sequence of one or more pipelines separated by one of the operators `;`, `&`, `&&`, or `||`. The
`if` conditions shown below are just special cases of `commands1`:

1.  `if [ condition ]`

    This is the traditional shell `test` command. It is available on all POSIX shells. The test
    command sets an exit code and the `if` statement acts accordingly. Typical tests are whether a
    file exists or one number is equal to another.

2.  `if [[ condition ]]`

    This is a new upgraded variation on `test` from _ksh_ that _bash_ and _zsh_ also support. This
    `test` command also sets an exit code and the `if` statement acts accordingly. Among its
    extended features, it can test whether a string matches a regular expression.

3.  `if ((condition))`

    Another _ksh_ extension that _bash_ and _zsh_ also support. This performs arithmetic. As the
    result of the arithmetic, an exit code is set and the `if` statement acts accordingly. It
    returns an exit code of zero (true) if the result of the arithmetic calculation is nonzero. Like
    `[[...]]`, this form is not POSIX and therefore not portable.

4.  `if (command)`

    This runs command in a subshell. When command completes, it sets an exit code and the `if`
    statement acts accordingly.

    A typical reason for using a subshell like this is to limit side-effects of `command` if
    `command` required variable assignments or other changes to the shell's environment. Such
    changes do not remain after the subshell completes.

5.  `if command`

    command is executed and the `if` statement acts according to its exit code.

# Other comparison operators

https://www.tldp.org/LDP/abs/html/comparison-ops.html

A _binary_ comparison operator compares two variables or quantities. _Note that integer and string
comparison use a different set of operators._

## **integer comparison**

1. `-eq` is equal to `if [ "$a" -eq "$b" ]`
2. `-ne` is not equal to `if [ "$a" -ne "$b" ]`
3. `-gt` is greater than `if [ "$a" -gt "$b" ]`
4. `-ge` is greater than or equal to `if [ "$a" -ge "$b" ]`
5. `-lt` is less than `if [ "$a" -lt "$b" ]`
6. `-le` is less than or equal to `if [ "$a" -le "$b" ]`
7. `<` is less than (within double parentheses) `(( "$a" < "$b" ))`
8. `<=` is less than or equal to (within double parentheses) `(("$a" <= "$b"))`
9. `>` is greater than (within double parentheses) `(("$a" > "$b"))`
10. `>=` is greater than or equal to (within double parentheses) `(("$a" >= "$b"))`

## String comparison

1. `=` is equal to `if [ "$a" = "$b" ]`

   > Note the whitespace framing the `=`. if [ "$a"="$b" ] is not equivalent to the above.

2. `==` is equal to `if [ "$a" == "$b" ]`

   This is a synonym for `=`.

   The `==` comparison operator behaves differently within a double-brackets test than within single
   brackets.

   ```sh
   [[ $a == z* ]]   # True if $a starts with an "z" (pattern matching).
   [[ $a == "z*" ]] # True if $a is equal to z* (literal matching).

   [ $a == z* ]     # File globbing and word splitting take place.
   [ "$a" == "z*" ] # True if $a is equal to z* (literal matching).
   ```

3. `!`= is not equal to `if [ "$a" != "$b" ]` This operator uses pattern matching within a
   `[[ ... ]]` construct.
4. `<` is less than, in ASCII alphabetical order `if [[ "$a" < "$b" ]]` `if [ "$a" \< "$b" ]` Note
   that the "<" needs to be escaped within a [ ] construct.
5. `>` is greater than, in ASCII alphabetical order `if [[ "$a" > "$b" ]]` `if [ "$a" \> "$b" ]`

   Note that the ">" needs to be escaped within a [ ] construct. See
   [ Example 27-11 ](https://www.tldp.org/LDP/abs/html/arrays.html#BUBBLE) for an application of
   this comparison operator.

6. `-z` string is null, that is, has zero length

   ```sh
   String=''   # Zero-length ("null") string variable.

   if [ -z "$String" ]
   then
   echo "\$String is null."
   else
   echo "\$String is NOT null."
   fi     # $String is null.
   ```

7. `-n` string is not null.

   > The `-n` test requires that the string be quoted within the test brackets. Using an unquoted
   > string with `! -z`, or even just the unquoted string alone within test brackets (see Example
   > 7-6) normally works, however, this is an unsafe practice. Always quote a tested string. [1]

## Compound comparison

1. `-a` logical and `exp1 -a exp2` returns true if both exp1 and exp2 are true.

2. `-o` logical or `exp1 -o exp2` returns true if either exp1 or exp2 is true.

   These are similar to the Bash comparison operators `&&` and `||`, used within double brackets.
   `[[ condition1 && condition2 ]]` The `-o` and `-a` operators work with the test command or occur
   within single test brackets.

   ```sh
   if [ "$expr1" -a "$expr2" ]
   then
   echo "Both expr1 and expr2 are true."
   else
   echo "Either expr1 or expr2 is false."
   fi
   ```

   > Note: But, as rihad points out:

   ```sh
   [ 1 -eq 1 ] && [ -n "`echo true 1>&2`" ]   # true
   [ 1 -eq 2 ] && [ -n "`echo true 1>&2`" ]   # (no output)
   # ^^^^^^^ False condition. So far, everything as expected.

   # However ...
   [ 1 -eq 2 -a -n "`echo true 1>&2`" ]       # true
   # ^^^^^^^ False condition. So, why "true" output?

   # Is it because both condition clauses within brackets evaluate?
   [[ 1 -eq 2 && -n "`echo true 1>&2`" ]]     # (no output)
   # No, that's not it.

   # Apparently && and || "short-circuit" while -a and -o do not.
   ```

## Example 7-5. Arithmetic and string comparisons

```sh
#!/bin/bash

a=4
b=5

#  Here "a" and "b" can be treated either as integers or strings.
#  There is some blurring between the arithmetic and string comparisons,
#+ since Bash variables are not strongly typed.

#  Bash permits integer operations and comparisons on variables
#+ whose value consists of all-integer characters.
#  Caution advised, however.

echo

if [ "$a" -ne "$b" ]
then
  echo "$a is not equal to $b"
  echo "(arithmetic comparison)"
fi

echo

if [ "$a" != "$b" ]
then
  echo "$a is not equal to $b."
  echo "(string comparison)"
  #     "4"  != "5"
  # ASCII 52 != ASCII 53
fi

# In this particular instance, both "-ne" and "!=" work.

echo

exit 0
```

## Example 7-6. Testing whether a string is null

```sh
#!/bin/bash
#  str-test.sh: Testing null strings and unquoted strings,
#+ but not strings and sealing wax, not to mention cabbages and kings . . .

# Using   if [ ... ]

# If a string has not been initialized, it has no defined value.
# This state is called "null" (not the same as zero!).

if [ -n $string1 ]    # string1 has not been declared or initialized.
then
  echo "String \"string1\" is not null."
else
  echo "String \"string1\" is null."
fi                    # Wrong result.
# Shows $string1 as not null, although it was not initialized.

echo

# Let's try it again.

if [ -n "$string1" ]  # This time, $string1 is quoted.
then
  echo "String \"string1\" is not null."
else
  echo "String \"string1\" is null."
fi                    # Quote strings within test brackets!

echo

if [ $string1 ]       # This time, $string1 stands naked.
then
  echo "String \"string1\" is not null."
else
  echo "String \"string1\" is null."
fi                    # This works fine.
# The [ ... ] test operator alone detects whether the string is null.
# However it is good practice to quote it (if [ "$string1" ]).
#
# As Stephane Chazelas points out,
#    if [ $string1 ]    has one argument, "]"
#    if [ "$string1" ]  has two arguments, the empty "$string1" and "]"


echo


string1=initialized

if [ $string1 ]       # Again, $string1 stands unquoted.
then
  echo "String \"string1\" is not null."
else
  echo "String \"string1\" is null."
fi                    # Again, gives correct result.
# Still, it is better to quote it ("$string1"), because . . .


string1="a = b"

if [ $string1 ]       # Again, $string1 stands unquoted.
then
  echo "String \"string1\" is not null."
else
  echo "String \"string1\" is null."
fi                    # Not quoting "$string1" now gives wrong result!

exit 0   # Thank you, also, Florian Wisser, for the "heads-up".
```

## Example 7-7. zmore

```sh
#!/bin/bash
# zmore

# View gzipped files with 'more' filter.

E_NOARGS=85
E_NOTFOUND=86
E_NOTGZIP=87

if [ $# -eq 0 ] # same effect as:  if [ -z "$1" ]
# $1 can exist, but be empty:  zmore "" arg2 arg3
then
  echo "Usage: `basename $0` filename" >&2
  # Error message to stderr.
  exit $E_NOARGS
  # Returns 85 as exit status of script (error code).
fi

filename=$1

if [ ! -f "$filename" ]   # Quoting $filename allows for possible spaces.
then
  echo "File $filename not found!" >&2   # Error message to stderr.
  exit $E_NOTFOUND
fi

if [ ${filename##*.} != "gz" ]
# Using bracket in variable substitution.
then
  echo "File $1 is not a gzipped file!"
  exit $E_NOTGZIP
fi

zcat $1 | more

# Uses the 'more' filter.
# May substitute 'less' if desired.

exit $?   # Script returns exit status of pipe.
#  Actually "exit $?" is unnecessary, as the script will, in any case,
#+ return the exit status of the last command executed.
```
