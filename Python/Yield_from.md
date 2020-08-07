#### Why yield from?

When a new feature is introduced in a programming language we should ask ourselves if and why this was necessary. The
short explanation is that it enables you to easily refactor a generator by splitting it up into multiple generators.

For basic purposes we can use plain generators to compute values and to pass those values around. The benefits of
`yield from` should become clear when we know what it does and in which situations it can be used.

Consider a generator that looks like this:

<pre class="code literal-block">
def generator():
    for i in range(10):
        yield i
    for j in range(10, 20):
        yield j
</pre>

As expected this generator yields the numbers 0 to 19. Let's say we wish to split this generator into two generators so
we reuse them elsewhere.

We could rewrite the above into:

<pre class="code literal-block">
def generator2():
    for i in range(10):
        yield i

def generator3():
    for j in range(10, 20):
        yield j

def generator():
    for i in generator2():
        yield i
    for j in generator3():
        yield j
</pre>

This version of `generator()` also yields the numbers 0 to 19. However, it feels unnecessary to specify that we wish to
iterate over both `generator2` and `generator3` and yield their values. This is where `yield from` comes in. Using this
new keyword we can rewrite `generator` into:

<pre class="code literal-block">
defgenerator():
    yield from generator2()
    yield from generator3()
</pre>

This gives the same result and it is much cleaner to write and maintain. It is also quite similar to the way functions
are refactored and split up into multiple functions. For example, a large function can be split into several smaller
functions, `f1()`, `f2()` and `f3()`, and the original function simply calls `f1()`, `f2()` and `f3()` in sequence.

#### Useful situations for 'yield from'

Those of you familiar with the `itertools` module may note that the above example is rather simple and does not truly
justify introducing a new keyword in the language.

Using the `chain` function from the `itertools` module we also could have written:

<pre class="code literal-block">
from itertools import chain

def generator():
    for v in chain(generator2(), generator3()):
        yield v
</pre>

It can be argued that the `yield from` syntax and semantics are slightly cleaner than importing an additional function
from a module but, leaving that aside, we have not yet seen an example where `yield from` has enabled us to do something
new. As we will see later in this tutorial, the main benefit of `yield from` is to allow easy refactoring of generators.

It should be noted that it is not necessary for new programming language syntax to also introduce new semantics (i.e, to
express something that was not possible before).

Many languages introduce syntax, often called syntactic sugar, to make it easier to write something that would otherwise
be cumbersome to write. For example, Haskell allows you to easily write a string as `"example"` which is shorthand
syntax for `['e', 'x', 'a', 'm', 'p', 'l', 'e']` (a list of characters). Cleaner and more maintainable code can suffice
to introduce new syntax into a programming language.
