Use the `nargs` option or the `'append'` setting of the `action` option (depending on how you want
the user interface to behave).

**nargs**

```python
parser.add_argument('-l','--list', nargs='+', help='<Required> Set flag', required=True)
# Use like:
# python arg.py -l 1234 2345 3456 4567
```

`nargs='+'` takes 1 or more arguments, `nargs='*'` takes zero or more.

**append**

```python
parser.add_argument('-l','--list', action='append', help='<Required> Set flag', required=True)
# Use like:
# python arg.py -l 1234 -l 2345 -l 3456 -l 4567
```

With `append` you provide the option multiple times to build up the list.

**Don't use `type=list`!!!** - There is probably no situation where you would want to use
`type=list` with `argparse`. Ever.

---

Let's take a look in more detail at some of the different ways one might try to do this, and the end
result.

```python
import argparse

parser = argparse.ArgumentParser()

# By default it will fail with multiple arguments.
parser.add_argument('--default')

# Telling the type to be a list will also fail for multiple arguments,
# but give incorrect results for a single argument.
parser.add_argument('--list-type', type=list)

# This will allow you to provide multiple arguments, but you will get
# a list of lists which is not desired.
parser.add_argument('--list-type-nargs', type=list, nargs='+')

# This is the correct way to handle accepting multiple arguments.
# '+' == 1 or more.
# '*' == 0 or more.
# '?' == 0 or 1.
# An int is an explicit number of arguments to accept.
parser.add_argument('--nargs', nargs='+')

# To make the input integers
parser.add_argument('--nargs-int-type', nargs='+', type=int)

# An alternate way to accept multiple inputs, but you must
# provide the flag once per input. Of course, you can use
# type=int here if you want.
parser.add_argument('--append-action', action='append')

# To show the results of the given option to screen.
for _, value in parser.parse_args()._get_kwargs():
    if value is not None:
        print(value)
```

Here is the output you can expect:

```
$ python arg.py --default 1234 2345 3456 4567
...
arg.py: error: unrecognized arguments: 2345 3456 4567

$ python arg.py --list-type 1234 2345 3456 4567
...
arg.py: error: unrecognized arguments: 2345 3456 4567

$ # Quotes won't help here...
$ python arg.py --list-type "1234 2345 3456 4567"
['1', '2', '3', '4', ' ', '2', '3', '4', '5', ' ', '3', '4', '5', '6', ' ', '4', '5', '6', '7']

$ python arg.py --list-type-nargs 1234 2345 3456 4567
[['1', '2', '3', '4'], ['2', '3', '4', '5'], ['3', '4', '5', '6'], ['4', '5', '6', '7']]

$ python arg.py --nargs 1234 2345 3456 4567
['1234', '2345', '3456', '4567']

$ python arg.py --nargs-int-type 1234 2345 3456 4567
[1234, 2345, 3456, 4567]

$ # Negative numbers are handled perfectly fine out of the box.
$ python arg.py --nargs-int-type -1234 2345 -3456 4567
[-1234, 2345, -3456, 4567]

$ python arg.py --append-action 1234 --append-action 2345 --append-action 3456 --append-action 4567
['1234', '2345', '3456', '4567']
```

_Takeaways_:

- Use `nargs` or `action='append'`

  - `nargs` can be more straightforward from a user perspective, but it can be unintuitive if there
    are positional arguments because `argparse` can't tell what should be a positional argument and
    what belongs to the `nargs`; if you have positional arguments then `action='append'` may end up
    being a better choice.
  - The above is only true if `nargs` is given `'*'`, `'+'`, or `'?'`. If you provide an integer
    number (such as `4`) then there will be no problem mixing options with `nargs` and positional
    arguments because `argparse` will know exactly how many values to expect for the option.

- Don't use quotes on the command line<sup>1</sup>
- Don't use `type=list`, as it will return a list of lists

  - This happens because under the hood `argparse` uses the value of `type` to coerce _each
    individual given argument_ you your chosen `type`, not the aggregate of all arguments.
  - You can use `type=int` (or whatever) to get a list of ints (or whatever)
