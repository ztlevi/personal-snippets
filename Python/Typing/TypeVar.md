## Using `TypeVar` and `Generic` as class template[¶](https://www.pythonsheets.com/notes/python-typing.html#using-typevar-and-generic-as-class-template "Permalink to this headline")

Like c++ `template <typename T> class`

```c++
#include<iostream>

template<typename T>
class Foo {
public:
    Foo(T foo) {
        foo_ = foo;
    }
    T Get() {
        return foo_;
    }
private:
    T foo_;
};

int main(int argc, char *argv[])
{
    Foo<int> f(123);
    std::cout << f.Get() << std::endl;
    return 0;
}
```

Define a generic class in Python

```python
from typing import Generic,TypeVar

T = TypeVar("T")

class Foo(Generic[T]):
    def __init__(self, foo: T) -> None:
        self.foo = foo

    def get(self) -> T:
        return self.foo

f: Foo[str] = Foo("Foo")
v: int = f.get()
```

output:

```sh
$ mypy --strict foo.py
foo.py:13: error: Incompatible types in assignment (expression has type "str", variable has type "int")
```

## Scoping rules for `TypeVar`[¶](https://www.pythonsheets.com/notes/python-typing.html#scoping-rules-for-typevar "Permalink to this headline")

- `TypeVar` used in different generic function will be inferred to be different types.

```python
from typing import TypeVarT = TypeVar("T")

def foo(x: T) -> T:
    return x

def bar(y: T) -> T:
    return y

a: int = foo(1)    # ok: T is inferred to be int
b: int = bar("2")  # error: T is inferred to be str
```

output:

```sh
$ mypy --strict foo.py
foo.py:12: error: Incompatible types in assignment (expression has type "str", variable has type "int")
```

- `TypeVar` used in a generic class will be inferred to be same types.

```python
from typing importTypeVar,Generic

T = TypeVar("T")

class Foo(Generic[T]):

    def foo(self, x: T) -> T:
        return x

    def bar(self, y: T) -> T:
        return y

f: Foo[int] = Foo()
a: int = f.foo(1)    # ok: T is inferred to be int
b: str = f.bar("2")  # error: T is expected to be int
```

output:

```sh
$ mypy --strict foo.py
foo.py:15: error: Incompatible types in assignment (expression has type "int", variable has type "str")
foo.py:15: error: Argument 1 to "bar" of "Foo" has incompatible type "str"; expected "int"
```

- `TypeVar` used in a method but did not match any parameters which declare in `Generic` can be inferred to be different
  types.

```python
from typing importTypeVar, Generic

T = TypeVar("T")
S = TypeVar("S")

class Foo(Generic[T]):    # S does not match params

    def foo(self, x: T, y: S) -> S:
        return y

    def bar(self, z: S) -> S:
        return z

f: Foo[int] = Foo()
a: str = f.foo(1, "foo")  # S is inferred to be str
b: int = f.bar(12345678)  # S is inferred to be int
```

output:

```sh
$  mypy --strict foo.py
```

- `TypeVar` should not appear in body of method/function if it is unbound type.

```python
from typingimport TypeVar, Generic

T = TypeVar("T")
S = TypeVar("S")

def foo(x: T) -> None:
    a: T = x    # ok
    b: S = 123  # error: invalid type
```

output:

```sh
$ mypy --strict foo.py
foo.py:8: error: Invalid type "foo.S"
```

## Restricting to a fixed set of possible types[¶](https://www.pythonsheets.com/notes/python-typing.html#restricting-to-a-fixed-set-of-possible-types "Permalink to this headline")

`T = TypeVar('T', ClassA, ...)` means we create a **type variable with a value restriction**.

```python
from typingimport TypeVar

# restrict T = int or T = float
T = TypeVar("T", int, float)

def add(x: T, y: T) -> T:
    return x + y

add(1, 2)
add(1., 2.)
add("1", 2)
add("hello", "world")
```

output:

```python
# mypy can detect wrong type
$ mypy --strict foo.py
foo.py:10: error: Value of type variable "T" of "add" cannot be "object"
foo.py:11: error: Value of type variable "T" of "add" cannot be "str"
```

## `TypeVar` with an upper bound[¶](https://www.pythonsheets.com/notes/python-typing.html#typevar-with-an-upper-bound "Permalink to this headline")

`T = TypeVar('T', bound=BaseClass)` means we create a **type variable with an upper bound**. The concept is similar to
**polymorphism** in c++.

```c++
#include<iostream>

class Shape {
public:
    Shape(double width, double height) {
        width_ = width;
        height_ = height;
    };
    virtual double Area() = 0;
protected:
    double width_;
    double height_;
};

class Rectangle: public Shape {
public:
    Rectangle(double width, double height)
    :Shape(width, height)
    {};

    double Area() {
        return width_ * height_;
    };
};

class Triangle: public Shape {
public:
    Triangle(double width, double height)
    :Shape(width, height)
    {};

    double Area() {
        return width_ * height_ / 2;
    };
};

double Area(Shape &s) {
    return s.Area();
}

int main(int argc, char *argv[])
{
    Rectangle r(1., 2.);
    Triangle t(3., 4.);

    std::cout << Area(r) << std::endl;
    std::cout << Area(t) << std::endl;
    return 0;
}
```

Like c++, create a base class and `TypeVar` which bounds to the base class. Then, static type checker will take every
subclass as type of base class.

```python
from typing import TypeVar

class Shape:
    def __init__(self, width: float, height: float) -> None:
        self.width = width
        self.height = height

    def area(self) -> float:
        return 0

class Rectangle(Shape):
    def area(self) -> float:
        width: float = self.width
        height: float = self.height
        return width * height

class Triangle(Shape):
    def area(self) -> float:
        width: float = self.width
        height: float = self.height
        return width * height / 2

S = TypeVar("S", bound=Shape)

def area(s: S) -> float: # use Shape here is fine
    return s.area()

r: Rectangle = Rectangle(1, 2)
t: Triangle = Triangle(3, 4)
i: int = 5566

print(area(r))
print(area(t))
print(area(i))
```

output:

```sh
$ mypy --strict foo.py
foo.py:40: error: Value of type variable "S" of "area" cannot be "int"
```
