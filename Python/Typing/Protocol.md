## Generic protocols[¶](https://mypy.readthedocs.io/en/stable/generics.html#generic-protocols "Permalink to this headline")

Mypy supports generic protocols (see also
[Protocols and structural subtyping](https://mypy.readthedocs.io/en/stable/protocols.html#protocol-types)). Several
[predefined protocols](https://mypy.readthedocs.io/en/stable/protocols.html#predefined-protocols) are generic, such as
[`Iterable[T]`](https://docs.python.org/3/library/typing.html#typing.Iterable "(in Python v3.8)"), and you can define
additional generic protocols. Generic protocols mostly follow the normal rules for generic classes. Example:

```python
from typing import TypeVar
from typing_extensions import Protocol

T = TypeVar('T')

class Box(Protocol[T]):
    content: T

def do_stuff(one: Box[str], other: Box[bytes]) -> None:
    ...

class StringWrapper:
    def __init__(self, content: str) -> None:
        self.content = content

class BytesWrapper:
    def __init__(self, content: bytes) -> None:
        self.content = content

do_stuff(StringWrapper('one'), BytesWrapper(b'other'))  # OK

x: Box[float] = ...
y: Box[int] = ...
x = y  # Error -- Box is invariant
```

**The main difference between generic protocols and ordinary generic classes is that mypy checks that the declared
variances of generic type variables in a protocol match how they are used in the protocol definition.** The protocol in
this example is rejected, since the type variable `T` is used covariantly as a return type, but the type variable is
invariant:

```python
from typing import TypeVar
from typing_extensions import Protocol

T = TypeVar('T')

class ReadOnlyBox(Protocol[T]):  # Error: covariant type variable expected
    def content(self) -> T: ...
```

This example correctly uses a covariant type variable:

```python
from typing import TypeVar
from typing_extensions import Protocol

T_co = TypeVar('T_co', covariant=True)

class ReadOnlyBox(Protocol[T_co]):  # OK
    def content(self) -> T_co: ...

ax: ReadOnlyBox[float] = ...
ay: ReadOnlyBox[int] = ...
ax = ay  # OK -- ReadOnlyBox is covariant
```

See [Variance of generic types](https://mypy.readthedocs.io/en/stable/generics.html#variance-of-generics) for more about
variance.

Generic protocols can also be recursive. Example:

```python
T= TypeVar('T')

class Linked(Protocol[T]):
    val: T
    def next(self) -> 'Linked[T]': ...

class L:
    val: int

    ...  # details omitted

    def next(self) -> 'L':
        ...  # details omitted

def last(seq: Linked[T]) -> T:
    ...  # implementation omitted

result = last(L())  # Inferred type of 'result' is 'int'
```

## Simple user-defined protocols[¶](https://mypy.readthedocs.io/en/stable/protocols.html#simple-user-defined-protocols "Permalink to this headline")

You can define your own protocol class by inheriting the special `Protocol` class:

```python
from typingimport Iterablefrom typing_extensions import Protocol

class SupportsClose(Protocol):
    def close(self) -> None:
       ...  # Empty method body (explicit '...')

class Resource:  # No SupportsClose base class!
    # ... some methods ...

    def close(self) -> None:
       self.resource.release()

def close_all(items: Iterable[SupportsClose]) -> None:
    for item in items:
        item.close()

close_all([Resource(), open('some/file')])  # Okay!
```

`Resource` is a subtype of the `SupportsClose` protocol since it defines a compatible `close` method. Regular file
objects returned by [`open()`](https://docs.python.org/3/library/functions.html#open "(in Python v3.8)") are similarly
compatible with the protocol, as they support `close()`.

Note

The `Protocol` base class is provided in the `typing_extensions` package for Python 2.7 and 3.4-3.7. Starting with
Python 3.8, `Protocol` is included in the `typing` module.

## Defining subprotocols and subclassing protocols[¶](https://mypy.readthedocs.io/en/stable/protocols.html#defining-subprotocols-and-subclassing-protocols "Permalink to this headline")

You can also define subprotocols. Existing protocols can be extended and merged using multiple inheritance. Example:

```python
# ... continuing from the previous example

class SupportsRead(Protocol):
    defread(self, amount: int) -> bytes: ...

class TaggedReadableResource(SupportsClose, SupportsRead, Protocol):
    label: str

class AdvancedResource(Resource):
    def __init__(self, label: str) -> None:
        self.label = label

    def read(self, amount: int) -> bytes:
        # some implementation
        ...

resource: TaggedReadableResource
resource = AdvancedResource('handle with care')  # OK
```

Note that inheriting from an existing protocol does not automatically turn the subclass into a protocol – it just
creates a regular (non-protocol) class or ABC that implements the given protocol (or protocols). The `Protocol` base
class must always be explicitly present if you are defining a protocol:

```python
class NotAProtocol(SupportsClose):  # This is NOT a protocol
    new_attr: int

class Concrete:
   new_attr: int = 0

   def close(self) -> None:
       ...

# Error: nominal subtyping used by default
x: NotAProtocol = Concrete()  # Error!
```

You can also include default implementations of methods in protocols. If you explicitly subclass these protocols you can
inherit these default implementations. Explicitly including a protocol as a base class is also a way of documenting that
your class implements a particular protocol, and it forces mypy to verify that your class implementation is actually
compatible with the protocol.

Note

You can use Python 3.6 variable annotations ([**PEP 526**](https://www.python.org/dev/peps/pep-0526)) to declare
protocol attributes. On Python 2.7 and earlier Python 3 versions you can use type comments and properties.

## Recursive protocols[¶](https://mypy.readthedocs.io/en/stable/protocols.html#recursive-protocols "Permalink to this headline")

Protocols can be recursive (self-referential) and mutually recursive. This is useful for declaring abstract recursive
collections such as trees and linked lists:

```python
fromtyping import TypeVar, Optional
fromtyping_extensions import Protocol

class TreeLike(Protocol):
    value: int

    @property
    def left(self) -> Optional['TreeLike']: ...

    @property
    def right(self) -> Optional['TreeLike']: ...

class SimpleTree:
    def __init__(self, value: int) -> None:
        self.value = value
        self.left: Optional['SimpleTree'] = None
        self.right: Optional['SimpleTree'] = None

root: TreeLike = SimpleTree(0)  # OK
```

## Using isinstance() with protocols[¶](https://mypy.readthedocs.io/en/stable/protocols.html#using-isinstance-with-protocols "Permalink to this headline")

You can use a protocol class with
[`isinstance()`](https://docs.python.org/3/library/functions.html#isinstance "(in Python v3.8)") if you decorate it with
the `@runtime_checkable` class decorator. The decorator adds support for basic runtime structural checks:

```python
from typing_extensions import Protocol, runtime_checkable

@runtime_checkable
class Portable(Protocol):
    handles: int

class Mug:
    def __init__(self) -> None:
        self.handles = 1

mug = Mug()
if isinstance(mug, Portable):
   use(mug.handles)  # Works statically and at runtime
```

[`isinstance()`](https://docs.python.org/3/library/functions.html#isinstance "(in Python v3.8)") also works with the
[predefined protocols](https://mypy.readthedocs.io/en/stable/protocols.html#predefined-protocols) in
[`typing`](https://docs.python.org/3/library/typing.html#module-typing "(in Python v3.8)") such as
[`Iterable`](https://docs.python.org/3/library/typing.html#typing.Iterable "(in Python v3.8)").

Note

[`isinstance()`](https://docs.python.org/3/library/functions.html#isinstance "(in Python v3.8)") with protocols is not
completely safe at runtime. For example, signatures of methods are not checked. The runtime implementation only checks
that all protocol members are defined.

## Callback protocols[¶](https://mypy.readthedocs.io/en/stable/protocols.html#callback-protocols "Permalink to this headline")

Protocols can be used to define flexible callback types that are hard (or even impossible) to express using the
[`Callable[...]`](https://docs.python.org/3/library/typing.html#typing.Callable "(in Python v3.8)") syntax, such as
variadic, overloaded, and complex generic callbacks. They are defined with a special
[`__call__`](https://docs.python.org/3/reference/datamodel.html#object.__call__ "(in Python v3.8)") member:

```python
from typing import Optional, Iterable, List
from typing_extensions import Protocol

class Combiner(Protocol):
    def __call__(self, \*vals: bytes, maxlen: Optional[int] = None) -> List[bytes]: ...

def batch_proc(data: Iterable[bytes], cb_results: Combiner) -> bytes:
    for item in data:
        ...

def good_cb(\*vals: bytes, maxlen: Optional[int] = None) -> List[bytes]:
    ...
def bad_cb(\*vals: bytes, maxitems: Optional[int]) -> List[bytes]:
    ...

batch_proc([], good_cb)  # OK
batch_proc([], bad_cb)   # Error! Argument 2 has incompatible type because of
                         # different name and kind in the callback
```

Callback protocols and [`Callable`](https://docs.python.org/3/library/typing.html#typing.Callable "(in Python v3.8)")
types can be used interchangeably. Keyword argument names in
[`__call__`](https://docs.python.org/3/reference/datamodel.html#object.__call__ "(in Python v3.8)") methods must be
identical, unless a double underscore prefix is used. For example:

```python
fromtyping import Callable, TypeVar
from typing_extensions import Protocol

T = TypeVar('T')

class Copy(Protocol):
    def __call__(self, __origin: T) -> T: ...

copy_a: Callable[[T], T]
copy_b: Copy

copy_a = copy_b  # OK
copy_b = copy_a  # Also OK
```

<footer>
    [Next ](https://mypy.readthedocs.io/en/stable/python2.html "Type checking Python 2 code")

            [ Previous](https://mypy.readthedocs.io/en/stable/class_basics.html "Class basics")

</footer>
