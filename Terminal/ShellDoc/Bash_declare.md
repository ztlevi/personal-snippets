# [Bash declare builtin command](https://www.computerhope.com/unix/bash/declare.htm)

## Description

In bash, variables can have a value (such as the number **3**). Optionally, variables can also be
assigned attributes (such as [**integer**](https://www.computerhope.com/jargon/e/eval.htm)).

For instance, a "[read-only](https://www.computerhope.com/jargon/r/readonly.htm)" variable
(**declare -r**) cannot be unset, and its value and other attributes cannot be modified. An "indexed
array" variable (**declare -a**) is an [array](https://www.computerhope.com/jargon/a/array.htm) of
values that are indexed by number, starting at zero. An "associative array" variable (**declare
-A**) is an array of key-value pairs whose values are indexed by a keyword. (For more information,
see [arrays in bash](https://www.computerhope.com/unix/ubash.htm#arrays)).

In addition to variables, bash functions can be assigned attributes which affect their behavior. See
the **-f** and **-F** options below for more information.

> Note: The **typeset** command is an alias for **declare**. The two can be used interchangeably in
> bash.

## Syntax

**declare** [**-a**][**-a**] [**-f**][**-f**] [**-g**][**-i**] [**-l**][**-n**] [**-r**][**-t**]
[**-u**][**-x**] [**-p**]_name_[**=**_value_]] [_name_[**=**_value_]] ...

### Options

The **declare** builtin command takes the following general options:

| Option | Description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| ------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| **-f** | All names are treated as the names of functions, not variables.                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| **-F** | When displaying information about a function, display only the function's name and attributes. Do not display the contents of the function.                                                                                                                                                                                                                                                                                                                                                      |
| **-g** | When **declare** is used within a shell function, the **-g** option causes all operations on variables to take effect in [global](https://www.computerhope.com/jargon/g/global.htm) scope. If not used in a shell function, **-g** has no effect.                                                                                                                                                                                                                                                |
| **-p** | When used with _name_ arguments, **-p** displays the options and attributes of each variable _name_, and **-f** or **-F** are ignored (functions are not described). <br> When used with options, but no names, **-p** displays the attributes and values, which match the other specified options, of all variables and functions. <br> When used with no options or names, **-p** displays the attributes and values of all variables and functions, or only functions if **-f** is specified. |

The remaining options, listed below, cause **declare** to set an attribute if the option letter is
preceded with a dash. If preceded with a plus sign, **declare** will unset the attribute instead.

| Option | Unset with | Description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
| ------ | ---------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **-a** |            | Declare the named items to be indexed arrays. This attribute cannot be unset.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| **-A** |            | Declare the named items to be associative arrays. This attribute cannot be unset.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| **-i** | **+i**     | Give the named items the **integer** attribute. Values assigned to the variable will be restricted to integer values. If a non-integer value is assigned, either an error is reported, or the value **0** (zero) is assigned instead. <br> If the assigned value is a [string](https://www.computerhope.com/jargon/s/string.htm) representing an arithmetic operation on integers (such as "**5+5**"), the result of the operation is assigned. If the result of the operation is not an integer, the integer floor is assigned (e.g., "**22/7**" assigns **3**). [Floating point](https://www.computerhope.com/jargon/f/floapoin.htm) values may not be used, and if so an error will be reported. Bash does not support floating point math operations. |
| **-l** | **+l**     | When a value is assigned to the named variable, convert all [uppercase](https://www.computerhope.com/jargon/u/uppercase.htm) letters to [lowercase](https://www.computerhope.com/jargon/l/lowercas.htm). If the uppercase (**-u**) attribute was previously set for the variable, the attribute is unset.                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
| **-n** | **+n**     | Give each _name_ the **nameref** attribute, making it a "name reference" to another variable. The referenced variable is defined by the value of _name_. All references, assignments, and attribute modifications to _name_, except for those using or changing the **nameref** attribute, are performed on the variable referenced by the value of _name_. In other words, it makes the variable a [pointer](https://www.computerhope.com/jargon/p/pointer.htm) to another variable. <br> The **nameref** attribute cannot be applied to arrays.                                                                                                                                                                                                         |
| **-r** | **+r**     | Make the named items read-only. They cannot subsequently be assigned values or unset.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| **-t** | **+t**     | Give each named item the **trace** attribute. If the item is a function, it will inherit the **DEBUG** and **RETURN** traps from the parent shell. If the item is a variable, the **trace** attribute has no effect.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| **-u** | **+u**     | When the named variable is assigned a value, any lowercase letters are converted to uppercase. If the lowercase (**-l**) attribute was previously set for the variable, the attribute is unset.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| **-x** | **+x**     | Mark named items for export to child processes, as if the [**export**](https://www.computerhope.com/unix/bash/export.htm) builtin command had been used.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |

### Lexical scope

When **declare** is used inside a shell function, all named items are declared in a local scope,
unless the **-g** option is used. This behavior is the same as using the **local** builtin command.

### Exit status

The exit status of **declare** is success (zero), unless an invalid option is specified or an error
occurs during variable assignment, in which case the status is failure (non-zero).

## Examples

### Declaring and listing variables

```
declare -p
```

List all declared variables, their attributes, and their values.

```
declare myvar
```

Declare a variable, **myvar**, and assign it no value. Now run the first command again:

```
declare -p
```

This time, notice in the output that **myvar** is listed after all other variables as:

```bash
declare -- myvar
```

The double dash is equivalent to "no options." This means that **myvar** is declared, and has no
attributes. If it had an assigned value, the value would be listed here.

```
myvar=33
```

Assign the value **33** to **myvar**.

```
declare myvar=33
```

Same as the above command.

```
declare -- myvar=33
```

Same as the above two commands.

```
declare -- myvar="33"
```

Same as the above three commands; this is the proper form, including the value being enclosed in
double-quotes. However, any of the above four commands have the same effect.

Now, list all variables again:

```
declare -p
```

You'll see that this time, the output lists **myvar** with its value assignment:

```bash
declare -- myvar="33"
```

The key thing to notice here is, when you run **declare -p**, variables are listed as the complete
**declare** command that you would need to set the variable to its current attributes and value.

### Setting and unsetting attributes

The next commands modify the variable's attributes:

```
declare -x myvar
```

Declare that **myvar** should be exported to any child shell processes. This is the equivalent of
using the **export** command:

```
export myvar
```

Now, list variable declarations:

```
declare -p
```

Notice that in the output, the double dash (no options) has been replaced with **-x**:

```bash
declare -x myvar="33"
```

To remove the export attribute, use **+x** instead:

```
declare +x myvar
```

After running the above command, **myvar** will no longer be exported to subshell processes.

### Integers, and integer evaluation

Bash allows you to declare a variable to have the **integer** attribute, which guarantees that the
variable will always hold an integer value. It also permits arithmetic evaluation when assigning a
value.

```
declare -i myvar
```

Declare that **myvar** should be treated an integer.

```
myvar="test"
```

The string "**test**" is a non-integer value, so the value **0** (zero) is assigned instead. You can
verify this if you [**echo**](https://www.computerhope.com/unix/uecho.htm) the value:

```
echo $myvar
```

```bash
0
```

Any positive or negative integer value can be assigned, though. (The maximum possible value depends
on your computer, but it's huge):

```
myvar=-33; echo $myvar
```

```bash
-33
```

If a string containing an arithmetic operation is assigned to an integer variable, the result of the
operation is assigned. For example:

```
myvar="2*11"; echo $myvar
```

```bash
22
```

The above command assigns **myvar** the value of **2** times **11**.

If the mathematical operation results in a number with a decimal point, the result is rounded down
to the next-lowest integer. For instance:

```
myvar="33/5"; echo $myvar
```

```bash
6
```

The precise result of 33 divided by 5 (**33/5**) is **6.6**, but bash rounds it down to the integer
**6**.

If you try to use decimal values as [operands](https://www.computerhope.com/jargon/o/operand.htm),
bash will return an error, stating that the decimal point is an unknown
[operator](https://www.computerhope.com/jargon/o/operator.htm):

```
myvar="33.1/5"; echo $myvar
```

```bash
bash: 33.1/5: syntax error: invalid arithmetic operator (error token is ".1/5")
```

To unset the integer attribute, use the option **+i**:

```
declare +i myvar
```

Now, **myvar** no longer has the **integer** attribute. Strings containing mathematical operations
will now be assigned to **myvar** [literally](https://www.computerhope.com/jargon/l/literal.htm),
rather than [evaluated](https://www.computerhope.com/jargon/e/eval.htm). For example:

```
myvar="33.1/5"; echo $myvar
```

```bash
33.1/5
```

### Indexed arrays

To declare an indexed array, use **-a**:

```
declare -a myvar
```

If **myvar** already had an assigned value, this value is indexed as the first element, numbered
zero:

```
echo ${myvar[0]}
```

```bash
33.1/5
```

You can now assign values to elements of the array **myvar** using integers as the indices:

```
myvar[1]="Hello"
```

```
myvar[2]="World!"
```

The elements can be accessed by index number using the following syntax:

```
echo ${myvar[2]}
```

```bash
World!
```

If you use a negative integer as the index, bash counts from the last element, rather than the
first. To access the last element, use index **-1**:

```
echo ${myvar[-1]}
```

```bash
World!
```

An index of **-2** would access the second-to-last array element, etc.:

```
echo ${myvar[-2]}
```

```bash
Hello
```

To display all elements in an array, you can use an asterisk ("**\***") as the index. Individual
elements are separated with a space:

```bash
echo ${myvar[*]}
```

```bash
33.1/5 Hello World!
```

> Note: You cannot unset the array attribute with **+a**. Bash has no procedure for converting an
> array to another type, so it will return an error, even if no values were assigned. For example:

```
declare +a myvar
```

```bash
bash: declare: myvar: cannot destroy array variables in this way
```

### Check if a variable is declared

If you're writing a bash script, and you need to check if a variable is declared, this is the proper
way to do it:

```bash
if [ -z ${myvar+x} ]; then echo "not set"; else echo "set"; fi
```

This will perform
["Assign Alternate Value" parameter expansion](https://www.computerhope.com/unix/ubash.htm#parameter-expansion),
which tests if **myvar** has been set, and if its value is
[null](https://www.computerhope.com/jargon/n/null.htm). If **myvar** is unset, it will echo "**not
set**". Otherwise, it will echo "**set**". Note that the letter **x** can be anything you'd like,
but is required for the comparison to occur.

## Related commands

[**export**](https://www.computerhope.com/unix/bash/export.htm) — Mark variables and functions to be
exported to child processes.
