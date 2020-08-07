<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

**Table of Contents**

- [Generics[¶](https://mypy.readthedocs.io/en/stable/generics.html#generics "Permalink to this headline")](#genericshttpsmypyreadthedocsioenstablegenericshtmlgenerics-permalink-to-this-headline)
  - [Defining generic classes[¶](https://mypy.readthedocs.io/en/stable/generics.html#defining-generic-classes "Permalink to this headline")](#defining-generic-classeshttpsmypyreadthedocsioenstablegenericshtmldefining-generic-classes-permalink-to-this-headline)
  - [Generic class internals[¶](https://mypy.readthedocs.io/en/stable/generics.html#generic-class-internals "Permalink to this headline")](#generic-class-internalshttpsmypyreadthedocsioenstablegenericshtmlgeneric-class-internals-permalink-to-this-headline)
  - [Defining sub-classes of generic classes[¶](https://mypy.readthedocs.io/en/stable/generics.html#defining-sub-classes-of-generic-classes "Permalink to this headline")](#defining-sub-classes-of-generic-classeshttpsmypyreadthedocsioenstablegenericshtmldefining-sub-classes-of-generic-classes-permalink-to-this-headline)
  - [Generic functions[¶](https://mypy.readthedocs.io/en/stable/generics.html#generic-functions "Permalink to this headline")](#generic-functionshttpsmypyreadthedocsioenstablegenericshtmlgeneric-functions-permalink-to-this-headline)
  - [Generic methods and generic self[¶](https://mypy.readthedocs.io/en/stable/generics.html#generic-methods-and-generic-self "Permalink to this headline")](#generic-methods-and-generic-selfhttpsmypyreadthedocsioenstablegenericshtmlgeneric-methods-and-generic-self-permalink-to-this-headline)
  - [Variance of generic types[¶](https://mypy.readthedocs.io/en/stable/generics.html#variance-of-generic-types "Permalink to this headline")](#variance-of-generic-typeshttpsmypyreadthedocsioenstablegenericshtmlvariance-of-generic-types-permalink-to-this-headline)
  - [Type variables with value restriction[¶](https://mypy.readthedocs.io/en/stable/generics.html#type-variables-with-value-restriction "Permalink to this headline")](#type-variables-with-value-restrictionhttpsmypyreadthedocsioenstablegenericshtmltype-variables-with-value-restriction-permalink-to-this-headline)
  - [Type variables with upper bounds[¶](https://mypy.readthedocs.io/en/stable/generics.html#type-variables-with-upper-bounds "Permalink to this headline")](#type-variables-with-upper-boundshttpsmypyreadthedocsioenstablegenericshtmltype-variables-with-upper-bounds-permalink-to-this-headline)
  - [Declaring decorators[¶](https://mypy.readthedocs.io/en/stable/generics.html#declaring-decorators "Permalink to this headline")](#declaring-decoratorshttpsmypyreadthedocsioenstablegenericshtmldeclaring-decorators-permalink-to-this-headline)
    - [Decorator factories[¶](https://mypy.readthedocs.io/en/stable/generics.html#decorator-factories "Permalink to this headline")](#decorator-factorieshttpsmypyreadthedocsioenstablegenericshtmldecorator-factories-permalink-to-this-headline)
  - [Generic type aliases[¶](https://mypy.readthedocs.io/en/stable/generics.html#generic-type-aliases "Permalink to this headline")](#generic-type-aliaseshttpsmypyreadthedocsioenstablegenericshtmlgeneric-type-aliases-permalink-to-this-headline)

<!-- markdown-toc end -->

# Generics[¶](https://mypy.readthedocs.io/en/stable/generics.html#generics "Permalink to this headline")

This section explains how you can define your own generic classes that take one or more type parameters, similar to
built-in types such as `List[X]`. User-defined generics are a moderately advanced feature and you can get far without
ever using them – feel free to skip this section and come back later.

## Defining generic classes[¶](https://mypy.readthedocs.io/en/stable/generics.html#defining-generic-classes "Permalink to this headline")

The built-in collection classes are generic classes. Generic types have one or more type parameters, which can be
arbitrary types. For example, `Dict[int, str]` has the type parameters `int` and `str`, and `List[int]` has a type
parameter `int`.

Programs can also define new generic classes. Here is a very simple generic class that represents a stack:

```python
from typing import TypeVar,Generic

T = TypeVar('T')

class Stack(Generic[T]):
    def __init__(self) -> None:
        # Create an empty list with items of type T
        self.items: List[T] = []

    def push(self, item: T) -> None:
        self.items.append(item)

    def pop(self) -> T:
        return self.items.pop()

    def empty(self) -> bool:
        return not self.items
```

The `Stack` class can be used to represent a stack of any type: `Stack[int]`, `Stack[Tuple[int, str]]`, etc.

Using `Stack` is similar to built-in container types:

```python
# Construct an empty Stack[int] instance
stack = Stack[int]()
stack.push(2)
stack.pop()
stack.push('x')        # Type error
```

Type inference works for user-defined generic types as well:

```python
def process(stack: Stack[int]) -> None: ...

process(Stack())   # Argument has inferred type Stack[int]
```

Construction of instances of generic types is also type checked:

```python
class Box(Generic[T]):
    def __init__(self, content: T) -> None:
        self.content = content

Box(1)  # OK, inferred type is Box[int]
Box[int](1)  # Also OK
s = 'some string'
Box[int](s)  # Type error
```

## Generic class internals[¶](https://mypy.readthedocs.io/en/stable/generics.html#generic-class-internals "Permalink to this headline")

You may wonder what happens at runtime when you index `Stack`. Actually, indexing `Stack` returns essentially a copy of
`Stack` that returns instances of the original class on instantiation:

```python
>>> print(Stack)
__main__.Stack
>>> print(Stack[int])
__main__.Stack[int]
>>> print(Stack[int]().__class__)
__main__.Stack
```

Note that built-in types [`list`](https://docs.python.org/3/library/stdtypes.html#list "(in Python v3.8)"),
[`dict`](https://docs.python.org/3/library/stdtypes.html#dict "(in Python v3.8)") and so on do not support indexing in
Python. This is why we have the aliases
[`List`](https://docs.python.org/3/library/typing.html#typing.List "(in Python v3.8)"),
[`Dict`](https://docs.python.org/3/library/typing.html#typing.Dict "(in Python v3.8)") and so on in the
[`typing`](https://docs.python.org/3/library/typing.html#module-typing "(in Python v3.8)") module. Indexing these
aliases gives you a class that directly inherits from the target class in Python:

```python
>>> from typing import List
>>> List[int]
typing.List[int]
>>> List[int].__bases__
(<class 'list'>, typing.MutableSequence)
```

Generic types could be instantiated or subclassed as usual classes, but the above examples illustrate that type
variables are erased at runtime. Generic `Stack` instances are just ordinary Python objects, and they have no extra
runtime overhead or magic due to being generic, other than a metaclass that overloads the indexing operator.

## Defining sub-classes of generic classes[¶](https://mypy.readthedocs.io/en/stable/generics.html#defining-sub-classes-of-generic-classes "Permalink to this headline")

User-defined generic classes and generic classes defined in
[`typing`](https://docs.python.org/3/library/typing.html#module-typing "(in Python v3.8)") can be used as base classes
for another classes, both generic and non-generic. For example:

```python
from typing import Generic, TypeVar, Mapping, Iterator, Dict

KT = TypeVar('KT')
VT = TypeVar('VT')

class MyMap(Mapping[KT, VT]):  # This is a generic subclass of Mapping
    def __getitem__(self, k: KT) -> VT:
        ...  # Implementations omitted
    def __iter__(self) -> Iterator[KT]:
        ...
    def __len__(self) -> int:
        ...

items: MyMap[str, int]  # Okay

class StrDict(Dict[str, str]):  # This is a non-generic subclass of Dict
    def __str__(self) -> str:
        return 'StrDict({})'.format(super().__str__())

data: StrDict[int, int]  # Error! StrDict is not generic
data2: StrDict  # OK

class Receiver(Generic[T]):
    def accept(self, value: T) -> None:
        ...

class AdvancedReceiver(Receiver[T]):
    ...
```

Note

You have to add an explicit [`Mapping`](https://docs.python.org/3/library/typing.html#typing.Mapping "(in Python v3.8)")
base class if you want mypy to consider a user-defined class as a mapping (and
[`Sequence`](https://docs.python.org/3/library/typing.html#typing.Sequence "(in Python v3.8)") for sequences, etc.).
This is because mypy doesn’t use _structural subtyping_ for these ABCs, unlike simpler protocols like
[`Iterable`](https://docs.python.org/3/library/typing.html#typing.Iterable "(in Python v3.8)"), which use
[structural subtyping](https://mypy.readthedocs.io/en/stable/protocols.html#protocol-types).

[`Generic`](https://docs.python.org/3/library/typing.html#typing.Generic "(in Python v3.8)") can be omitted from bases
if there are other base classes that include type variables, such as `Mapping[KT, VT]` in the above example. If you
include `Generic[...]` in bases, then it should list all type variables present in other bases (or more, if needed). The
order of type variables is defined by the following rules:

- If `Generic[...]` is present, then the order of variables is always determined by their order in `Generic[...]`.
- If there are no `Generic[...]` in bases, then all type variables are collected in the lexicographic order (i.e. by
  first appearance).

For example:

```python
from typing import Generic, TypeVar, Any

T = TypeVar('T')
S = TypeVar('S')
U = TypeVar('U')

class One(Generic[T]): ...
class Another(Generic[T]): ...

class First(One[T], Another[S]): ...
class Second(One[T], Another[S], Generic[S, U, T]): ...

x: First[int, str]        # Here T is bound to int, S is bound to str
y: Second[int, str, Any]  # Here T is Any, S is int, and U is str
```

## Generic functions[¶](https://mypy.readthedocs.io/en/stable/generics.html#generic-functions "Permalink to this headline")

Generic type variables can also be used to define generic functions:

```python
from typing import TypeVar, Sequence

T = TypeVar('T')      # Declare type variable

def first(seq: Sequence[T]) -> T:   # Generic function
    return seq[0]
```

As with generic classes, the type variable can be replaced with any type. That means `first` can be used with any
sequence type, and the return type is derived from the sequence item type. For example:

```python
# Assume first defined as above.

s = first('foo')      # s has type str.
n = first([1, 2, 3])  # n has type int.
```

Note also that a single definition of a type variable (such as `T` above) can be used in multiple generic functions or
classes. In this example we use the same type variable in two generic functions:

```python
from typing import TypeVar, Sequence

T = TypeVar('T')      # Declare type variable

def first(seq: Sequence[T]) -> T:
    return seq[0]

def last(seq: Sequence[T]) -> T:
    return seq[-1]
```

A variable cannot have a type variable in its type unless the type variable is bound in a containing generic class or
function.

## Generic methods and generic self[¶](https://mypy.readthedocs.io/en/stable/generics.html#generic-methods-and-generic-self "Permalink to this headline")

You can also define generic methods — just use a type variable in the method signature that is different from class type
variables. In particular, `self` may also be generic, allowing a method to return the most precise type known at the
point of access.

Note

This feature is experimental. Checking code with type annotations for self arguments is still not fully implemented.
Mypy may disallow valid code or allow unsafe code.

In this way, for example, you can typecheck chaining of setter methods:

```python
from typing import TypeVar

T = TypeVar('T', bound='Shape')

class Shape:
    def set_scale(self: T, scale: float) -> T:
        self.scale = scale
        return self

class Circle(Shape):
    def set_radius(self, r: float) -> 'Circle':
        self.radius = r
        return self

class Square(Shape):
    def set_width(self, w: float) -> 'Square':
        self.width = w
        return self

circle = Circle().set_scale(0.5).set_radius(2.7)  # type: Circle
square = Square().set_scale(0.5).set_width(3.2)  # type: Square
```

Without using generic `self`, the last two lines could not be type-checked properly.

Other uses are factory methods, such as copy and deserialization. For class methods, you can also define generic `cls`,
using [`Type[T]`](https://docs.python.org/3/library/typing.html#typing.Type "(in Python v3.8)"):

```python
from typing import TypeVar, Tuple, Type

T = TypeVar('T', bound='Friend')

class Friend:
    other = None  # type: Friend

    @classmethod
    def make_pair(cls: Type[T]) -> Tuple[T, T]:
        a, b = cls(), cls()
        a.other = b
        b.other = a
        return a, b

class SuperFriend(Friend):
    pass

a, b = SuperFriend.make_pair()
```

Note that when overriding a method with generic `self`, you must either return a generic `self` too, or return an
instance of the current class. In the latter case, you must implement this method in all future subclasses.

Note also that mypy cannot always verify that the implementation of a copy or a deserialization method returns the
actual type of self. Therefore you may need to silence mypy inside these methods (but not at the call site), possibly by
making use of the `Any` type.

For some advanced uses of self-types see
[additional examples](https://mypy.readthedocs.io/en/stable/more_types.html#advanced-self).

## Variance of generic types[¶](https://mypy.readthedocs.io/en/stable/generics.html#variance-of-generic-types "Permalink to this headline")

There are three main kinds of generic types with respect to subtype relations between them: invariant, covariant, and
contravariant. Assuming that we have a pair of types `A` and `B`, and `B` is a subtype of `A`, these are defined as
follows:

- A generic class `MyCovGen[T, ...]` is called covariant in type variable `T` if `MyCovGen[B, ...]` is always a subtype
  of `MyCovGen[A, ...]`.
- A generic class `MyContraGen[T, ...]` is called contravariant in type variable `T` if `MyContraGen[A, ...]` is always
  a subtype of `MyContraGen[B, ...]`.
- A generic class `MyInvGen[T, ...]` is called invariant in `T` if neither of the above is true.

Let us illustrate this by few simple examples:

- [`Union`](https://docs.python.org/3/library/typing.html#typing.Union "(in Python v3.8)") is covariant in all
  variables: `Union[Cat, int]` is a subtype of `Union[Animal, int]`, `Union[Dog, int]` is also a subtype of
  `Union[Animal, int]`, etc. Most immutable containers such as
  [`Sequence`](https://docs.python.org/3/library/typing.html#typing.Sequence "(in Python v3.8)") and
  [`FrozenSet`](https://docs.python.org/3/library/typing.html#typing.FrozenSet "(in Python v3.8)") are also covariant.

- [`Callable`](https://docs.python.org/3/library/typing.html#typing.Callable "(in Python v3.8)") is an example of type
  that behaves contravariant in types of arguments, namely `Callable[[Employee], int]` is a subtype of
  `Callable[[Manager], int]`. To understand this, consider a function:

  ```python
  def salaries(staff: List[Manager],
               accountant: Callable[[Manager], int]) -> List[int]: ...
  ```

  This function needs a callable that can calculate a salary for managers, and if we give it a callable that can
  calculate a salary for an arbitrary employee, it’s still safe.

- [`List`](https://docs.python.org/3/library/typing.html#typing.List "(in Python v3.8)") is an invariant generic type.
  Naively, one would think that it is covariant, but let us consider this code:

  ```python
  class Shape:
      pass

  class Circle(Shape):
      def rotate(self):
          ...

  def add_one(things: List[Shape]) -> None:
      things.append(Shape()) # since it's mutable, we add Shape objects who doesn't have a rotate function defined

  my_things: List[Circle] = []
  add_one(my_things)     # This may appear safe, but...
  my_things[0].rotate()  # ...this will fail
  ```

  Another example of invariant type is
  [`Dict`](https://docs.python.org/3/library/typing.html#typing.Dict "(in Python v3.8)"). Most mutable containers are
  invariant.

By default, mypy assumes that all user-defined generics are invariant. To declare a given generic class as covariant or
contravariant use type variables defined with special keyword arguments `covariant` or `contravariant`. For example:

```python
from typing import Generic, TypeVar

T_co = TypeVar('T_co', covariant=True)

class Box(Generic[T_co]):  # this type is declared covariant
    def __init__(self, content: T_co) -> None:
        self._content = content

    def get_content(self) -> T_co:
        return self._content

def look_into(box: Box[Animal]): ...

my_box = Box(Cat())
look_into(my_box)  # OK, but mypy would complain here for an invariant type
```

## Type variables with value restriction[¶](https://mypy.readthedocs.io/en/stable/generics.html#type-variables-with-value-restriction "Permalink to this headline")

By default, a type variable can be replaced with any type. However, sometimes it’s useful to have a type variable that
can only have some specific types as its value. A typical example is a type variable that can only have values `str` and
`bytes`:

```python
from typing import TypeVar

AnyStr = TypeVar('AnyStr', str, bytes)
```

This is actually such a common type variable that
[`AnyStr`](https://docs.python.org/3/library/typing.html#typing.AnyStr "(in Python v3.8)") is defined in
[`typing`](https://docs.python.org/3/library/typing.html#module-typing "(in Python v3.8)") and we don’t need to define
it ourselves.

We can use [`AnyStr`](https://docs.python.org/3/library/typing.html#typing.AnyStr "(in Python v3.8)") to define a
function that can concatenate two strings or bytes objects, but it can’t be called with other argument types:

```python
from typing import AnyStr

def concat(x: AnyStr, y: AnyStr) -> AnyStr:
    return x + y

concat('a', 'b')    # Okay
concat(b'a', b'b')  # Okay
concat(1, 2)        # Error!
```

Note that this is different from a union type, since combinations of `str` and `bytes` are not accepted:

```python
concat('string', b'bytes')   # Error!
```

In this case, this is exactly what we want, since it’s not possible to concatenate a string and a bytes object! The type
checker will reject this function:

```python
def union_concat(x: Union[str, bytes], y: Union[str, bytes]) -> Union[str, bytes]:
    return x + y  # Error: can't concatenate str and bytes
```

Another interesting special case is calling `concat()` with a subtype of `str`:

```python
class S(str): pass

ss = concat(S('foo'), S('bar'))
```

You may expect that the type of `ss` is `S`, but the type is actually `str`: a subtype gets promoted to one of the valid
values for the type variable, which in this case is `str`. This is thus subtly different from _bounded quantification_
in languages such as Java, where the return type would be `S`. The way mypy implements this is correct for `concat`,
since `concat` actually returns a `str` instance in the above example:

```python
>>> print(type(ss))
<class 'str'>
```

You can also use a [`TypeVar`](https://docs.python.org/3/library/typing.html#typing.TypeVar "(in Python v3.8)") with a
restricted set of possible values when defining a generic class. For example, mypy uses the type
[`Pattern[AnyStr]`](https://docs.python.org/3/library/typing.html#typing.Pattern "(in Python v3.8)") for the return
value of [`re.compile()`](https://docs.python.org/3/library/re.html#re.compile "(in Python v3.8)"), since regular
expressions can be based on a string or a bytes pattern.

## Type variables with upper bounds[¶](https://mypy.readthedocs.io/en/stable/generics.html#type-variables-with-upper-bounds "Permalink to this headline")

A type variable can also be restricted to having values that are subtypes of a specific type. This type is called the
upper bound of the type variable, and is specified with the `bound=...` keyword argument to
[`TypeVar`](https://docs.python.org/3/library/typing.html#typing.TypeVar "(in Python v3.8)").

```python
from typing import TypeVar, SupportsAbs

T = TypeVar('T', bound=SupportsAbs[float])
```

In the definition of a generic function that uses such a type variable `T`, the type represented by `T` is assumed to be
a subtype of its upper bound, so the function can use methods of the upper bound on values of type `T`.

```python
def largest_in_absolute_value(*xs: T) -> T:
    return max(xs, key=abs)  # Okay, because T is a subtype of SupportsAbs[float].
```

In a call to such a function, the type `T` must be replaced by a type that is a subtype of its upper bound. Continuing
the example above,

```python
largest_in_absolute_value(-3.5, 2)   # Okay, has type float.
largest_in_absolute_value(5+6j, 7)   # Okay, has type complex.
largest_in_absolute_value('a', 'b')  # Error: 'str' is not a subtype of SupportsAbs[float].
```

Type parameters of generic classes may also have upper bounds, which restrict the valid values for the type parameter in
the same way.

A type variable may not have both a value restriction (see
[Type variables with value restriction](https://mypy.readthedocs.io/en/stable/generics.html#type-variable-value-restriction))
and an upper bound.

## Declaring decorators[¶](https://mypy.readthedocs.io/en/stable/generics.html#declaring-decorators "Permalink to this headline")

One common application of type variable upper bounds is in declaring a decorator that preserves the signature of the
function it decorates, regardless of that signature.

Note that class decorators are handled differently than function decorators in mypy: decorating a class does not erase
its type, even if the decorator has incomplete type annotations.

Here’s a complete example of a function decorator:

```python
from typing import Any, Callable, TypeVar, Tuple, cast

F = TypeVar('F', bound=Callable[..., Any])

# A decorator that preserves the signature.
def my_decorator(func: F) -> F:
    def wrapper(*args, **kwds):
        print("Calling", func)
        return func(*args, **kwds)
    return cast(F, wrapper)

# A decorated function.
@my_decorator
def foo(a: int) -> str:
    return str(a)

a = foo(12)
reveal_type(a)  # str
foo('x')    # Type check error: incompatible type "str"; expected "int"
```

From the final block we see that the signatures of the decorated functions `foo()` and `bar()` are the same as those of
the original functions (before the decorator is applied).

The bound on `F` is used so that calling the decorator on a non-function (e.g. `my_decorator(1)`) will be rejected.

Also note that the `wrapper()` function is not type-checked. Wrapper functions are typically small enough that this is
not a big problem. This is also the reason for the
[`cast()`](https://docs.python.org/3/library/typing.html#typing.cast "(in Python v3.8)") call in the `return` statement
in `my_decorator()`. See [Casts and type assertions](https://mypy.readthedocs.io/en/stable/casts.html#casts).

### Decorator factories[¶](https://mypy.readthedocs.io/en/stable/generics.html#decorator-factories "Permalink to this headline")

Functions that take arguments and return a decorator (also called second-order decorators), are similarly supported via
generics:

```python
from typing import Any, Callable, TypeVar

F = TypeVar('F', bound=Callable[..., Any])

def route(url: str) -> Callable[[F], F]:
    ...

@route(url='/')
def index(request: Any) -> str:
    return 'Hello world'
```

Sometimes the same decorator supports both bare calls and calls with arguments. This can be achieved by combining with
[`@overload`](https://docs.python.org/3/library/typing.html#typing.overload "(in Python v3.8)"):

```python
from typing import Any, Callable, TypeVar, overload

F = TypeVar('F', bound=Callable[..., Any])

# Bare decorator usage
@overload
def atomic(__func: F) -> F: ...
# Decorator with arguments
@overload
def atomic(*, savepoint: bool = True) -> Callable[[F], F]: ...

# Implementation
def atomic(__func: Callable[..., Any] = None, *, savepoint: bool = True):
    def decorator(func: Callable[..., Any]):
        ...  # Code goes here
    if __func is not None:
        return decorator(__func)
    else:
        return decorator

# Usage
@atomic
def func1() -> None: ...

@atomic(savepoint=False)
def func2() -> None: ...
```

## Generic type aliases[¶](https://mypy.readthedocs.io/en/stable/generics.html#generic-type-aliases "Permalink to this headline")

Type aliases can be generic. In this case they can be used in two ways: Subscripted aliases are equivalent to original
types with substituted type variables, so the number of type arguments must match the number of free type variables in
the generic type alias. Unsubscripted aliases are treated as original types with free variables replaced with `Any`.
Examples (following [**PEP 484: Type aliases**](https://www.python.org/dev/peps/pep-0484#type-aliases)):

```python
from typing import TypeVar, Iterable, Tuple, Union, Callable
S = TypeVar('S')

TInt = Tuple[int, S]
UInt = Union[S, int]
CBack = Callable[..., S]

def response(query: str) -> UInt[str]:  # Same as Union[str, int]
    ...
def activate(cb: CBack[S]) -> S:        # Same as Callable[..., S]
    ...
table_entry: TInt  # Same as Tuple[int, Any]

T = TypeVar('T', int, float, complex)

Vec = Iterable[Tuple[T, T]]

def inproduct(v: Vec[T]) -> T:
    return sum(x*y for x, y in v)

def dilate(v: Vec[T], scale: T) -> Vec[T]:
    return ((x * scale, y * scale) for x, y in v)

v1: Vec[int] = []      # Same as Iterable[Tuple[int, int]]
v2: Vec = []           # Same as Iterable[Tuple[Any, Any]]
v3: Vec[int, int] = [] # Error: Invalid alias, too many type arguments!
```

Type aliases can be imported from modules just like other names. An alias can also target another alias, although
building complex chains of aliases is not recommended – this impedes code readability, thus defeating the purpose of
using aliases. Example:

```python
from typing import TypeVar,Generic, Optional
from example1 import AliasType
from example2 import Vec

# AliasType and Vec are type aliases (Vec as defined above)

def fun() -> AliasType:
    ...

T = TypeVar('T')

class NewVec(Vec[T]):
    ...

for i, j in NewVec[int]():
    ...

OIntVec = Optional[Vec[int]]
```

Note

A type alias does not define a new type. For generic type aliases this means that variance of type variables used for
alias definition does not apply to aliases. A parameterized generic alias is treated simply as an original type with the
corresponding type variables substituted.
