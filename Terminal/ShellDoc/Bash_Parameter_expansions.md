# [Bash Parameter Expansion](https://wiki.bash-hackers.org/syntax/pe)

## Basics

```bash
name="John"
echo ${name}
echo ${name/J/j}    #=> "john" (substitution)
echo ${name:0:2}    #=> "Jo" (slicing)
echo ${name::2}     #=> "Jo" (slicing)
echo ${name::-1}    #=> "Joh" (slicing)
echo ${name:(-1)}   #=> "n" (slicing from right)
echo ${name:(-2):1} #=> "h" (slicing from right)
echo ${food:-Cake}  #=> $food or "Cake"
length=2
echo ${name:0:length}  #=> "Jo"
```

## Indirection

```bash
value_1="aaa"
value_2="value_1"
printf 'The value of "%s" is: "%s"\n' "${value_2}" "${!value_2}"
# The value of "value_1" is: "hello"
```

## Substitution

```bash
${FOO%suffix}   # Remove suffix
${FOO#prefix}   # Remove prefix
${FOO%%suffix}   # Remove long suffix
${FOO##prefix}   # Remove long prefix
${FOO/from/to}   # Replace first match
${FOO//from/to}   # Replace all
${FOO/%from/to}   # Replace suffix
${FOO/#from/to}   # Replace prefix
```

Well, maybe the most common use for it is to **extract parts of a filename**. Just look at the following list with
examples:

- **Get name without extension**
  - `${FILENAME%.*}`
  - ⇒ bash_hackers~~.txt~~
- **Get extension**
  - `${FILENAME##*.}`
  - ⇒ ~~bash_hackers.~~txt
- **Get directory name**
  - `${PATHNAME%/*}`
  - ⇒ /home/bash~~/bash_hackers.txt~~
- **Get filename**
  - `${PATHNAME##*/}`
  - ⇒ ~~/home/bash/~~bash_hackers.txt

Other examples:

```bash
STR="/path/to/foo.cpp"
echo ${STR%.cpp}    # /path/to/foo
echo ${STR%.cpp}.o  # /path/to/foo.o

echo ${STR##*.}     # cpp (extension). Define the long prefix as *.
echo ${STR##*/}     # foo.cpp (basepath)

echo ${STR#*/}      # path/to/foo.cpp. Define the short prefix as */
echo ${STR##*/}     # foo.cpp

echo ${STR/foo/bar} # /path/to/bar.cpp


STR="Hello world"
echo ${STR:6:5}   # "world"
echo ${STR:-5:5}  # "world"


SRC="/path/to/foo.cpp"
BASE=${SRC##*/}   #=> "foo.cpp" (basepath). Define the long prefix as */
DIR=${SRC%$BASE}  #=> "/path/to/" (dirpath)
```

## Length

```bash
${#FOO}   # Length of $FOO
```

## Defaults

```bash
${FOO:-val}  # $FOO, or val if not set
${FOO:=val}  # Set $FOO to val if not set
${FOO:+val}  # val if $FOO is set
${FOO:?message}  # Show error message and exit if $FOO is not set
```

> Note: The : is optional (eg, \${FOO=word} works)

## Comments

```bash
# Single line comment
: '
This is a
multi line
comment
'
```

## Substrings

```bash
${FOO:0:3}  # Substring (position, length)
${FOO:-3:3}  # Substring from the right
```

## Manipulation

```bash
STR="HELLO WORLD!"
echo ${STR,}   #=> "hELLO WORLD!" (lowercase 1st letter)
echo ${STR,,}  #=> "hello world!" (all lowercase)

STR="hello world!"
echo ${STR^}   #=> "Hello world!" (uppercase 1st letter)
echo ${STR^^}  #=> "HELLO WORLD!" (all uppercase)

STR="hello world!"
echo ${STR~}   #=> "Hello world!" (reverse 1st letter)
echo ${STR~~}  #=> "HELLO WORLD!" (all reversed)
```

# ZSH Parameter Expansion (`man zshexpn`)

`h [ digits ]`: Remove a trailing pathname component, shortening the path by one directory level: this is the `head' of
the pathname.

```

${0:A:h} # => get the absolute parent path
```
