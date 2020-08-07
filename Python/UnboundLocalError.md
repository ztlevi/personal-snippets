## [](https://docs.python.org/3/faq/programming.html#id7)[¶](https://docs.python.org/3/faq/programming.html#core-language "Permalink to this headline")

### [Why am I getting an UnboundLocalError when the variable has a value?](https://docs.python.org/3/faq/programming.html#id8)[¶](https://docs.python.org/3/faq/programming.html#why-am-i-getting-an-unboundlocalerror-when-the-variable-has-a-value "Permalink to this headline")

It can be a surprise to get the UnboundLocalError in previously working code when it is modified by adding an assignment
statement somewhere in the body of a function.

This code:

<pre>
>>> x = 10
>>> def bar():
...     print(x)
>>> bar()
10
</pre>

works, but this code:

<pre>
>>> x = 10
>>> def foo():
...     print(x)
...     x += 1
</pre>

results in an UnboundLocalError:

<pre>
>>> foo()
Traceback (most recent call last):
  ...
UnboundLocalError: local variable 'x' referenced before assignment
</pre>

This is because when you make an assignment to a variable in a scope, that variable becomes local to that scope and
shadows any similarly named variable in the outer scope. Since the last statement in foo assigns a new value to `x`, the
compiler recognizes it as a local variable. Consequently when the earlier `print(x)` attempts to print the uninitialized
local variable and an error results.

In the example above you can access the outer scope variable by declaring it global:

<pre>
>>> x = 10
>>> def foobar():
...     global x
...     print(x)
...     x += 1
>>> foobar()
10
</pre>

This explicit declaration is required in order to remind you that (unlike the superficially analogous situation with
class and instance variables) you are actually modifying the value of the variable in the outer scope:

<pre>
>>> print(x)
11
</pre>

You can do a similar thing in a nested scope using the
[`nonlocal`](https://docs.python.org/3/reference/simple_stmts.html#nonlocal) keyword:

<pre>
>>> def foo():
...    x = 10
...    def bar():
...        nonlocal x
...        print(x)
...        x += 1
...    bar()
...    print(x)
>>> foo()
10
11
</pre>

### [What are the rules for local and global variables in Python?](https://docs.python.org/3/faq/programming.html#id9)[¶](https://docs.python.org/3/faq/programming.html#what-are-the-rules-for-local-and-global-variables-in-python "Permalink to this headline")

In Python, variables that are only referenced inside a function are implicitly global. If a variable is assigned a value
anywhere within the function’s body, it’s assumed to be a local unless explicitly declared as global.

Though a bit surprising at first, a moment’s consideration explains this. On one hand, requiring
[`global`](https://docs.python.org/3/reference/simple_stmts.html#global) for assigned variables provides a bar against
unintended side-effects. On the other hand, if `global` was required for all global references, you’d be using `global`
all the time. You’d have to declare as global every reference to a built-in function or to a component of an imported
module. This clutter would defeat the usefulness of the `global` declaration for identifying side-effects.
