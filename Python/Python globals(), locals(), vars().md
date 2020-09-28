## https://stackoverflow.com/questions/7969949/whats-the-difference-between-globals-locals-and-vars

Each of these return a dictionary:

- globals() always returns the dictionary of the module namespace
- locals() always returns a dictionary of the current namespace
- vars() returns either a dictionary of the current namespace (if called with no argument) or the
  dictionary of the argument.

locals and vars could use some more explanation. if locals() is called inside a function it
constructs a dictionary of the function namespace as of that moment and returns it -- any further
name assignments are not reflected in the returned dictionary, and any assignments to the dictionary
are not reflected in the actual local namespace:

```
def test():
    a = 1
    b = 2
    huh = locals()
    c = 3
    print(huh)
    huh['d'] = 4
    print(d)
```

gives us:

```
{'a': 1, 'b': 2}
Traceback (most recent call last):
  File "test.py", line 30, in <module>
    test()
  File "test.py", line 26, in test
    print(d)
NameError: global name 'd' is not defined
```

So far, everything I've said about locals() is also true for vars()... here's the difference: vars()
accepts a single object as its argument, and if you give it an object it returns the **dict** of
that object. If that object was not a function the **dict** returned is that object's namespace:

```
class Test(object):
    a = 'one'
    b = 'two'
    def frobber(self):
        print self.c
t = Test()
huh = vars(t)
huh['c'] = 'three'
t.frobber()
```

which gives us:

```
three
```

If the object was a function, you still get its **dict**, but unless you're doing fun and
interesting stuff its probably not very useful:

```
def test():
    a = 1
    b = 2
    print test.c
huh = vars(test)       # these two lines are the same as 'test.c = 3'
huh['c'] = 3
test()
```
