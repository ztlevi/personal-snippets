## @overload[¶](https://www.pythonsheets.com/notes/python-typing.html#overload "Permalink to this headline")

Sometimes, we use `Union` to infer that the return of a function has multiple different types.
However, type checker cannot distinguish which type do we want. Therefore, following snippet shows
that type checker cannot determine which type is correct.

```python
from typing import List, Union

class Array(object):
    def __init__(self, arr: List[int]) -> None:
        self.arr = arr

    def __getitem__(self, i: Union[int, str]) -> Union[int, str]:
        if isinstance(i, int):
            return self.arr[i]
        if isinstance(i, str):
            return str(self.arr[int(i)])

arr = Array([1, 2, 3, 4, 5])
x:int = arr[1]
y:str = arr["2"]
```

output:

```sh
$ mypy --strict foo.py
foo.py:16: error: Incompatible types in assignment (expression has type "Union[int, str]", variable has type "int")
foo.py:17: error: Incompatible types in assignment (expression has type "Union[int, str]", variable has type "str")
```

Although we can use `cast` to solve the problem, it cannot avoid typo and `cast` is not safe.

```python
from typing import  List, Union,cast

class Array(object):
    def __init__(self, arr: List[int]) -> None:
        self.arr = arr

    def __getitem__(self, i: Union[int, str]) -> Union[int, str]:
        if isinstance(i, int):
            return self.arr[i]
        if isinstance(i, str):
            return str(self.arr[int(i)])

arr = Array([1, 2, 3, 4, 5])
x: int = cast(int, arr[1])
y: str = cast(str, arr[2])  # typo. we want to assign arr["2"]
```

output:

```sh
$ mypy --strict foo.py
$ echo $?
0
```

Using `@overload` can solve the problem. We can declare the return type explicitly.

```python
from typing import Generic,List,Union, overload

class Array(object):
    def __init__(self, arr: List[int]) -> None:
        self.arr = arr

    @overload
    def __getitem__(self, i: str) -> str:
        ...

    @overload
    def __getitem__(self, i: int) -> int:
        ...

    def __getitem__(self, i: Union[int, str]) -> Union[int, str]:
        if isinstance(i, int):
            return self.arr[i]
        if isinstance(i, str):
            return str(self.arr[int(i)])

arr = Array([1, 2, 3, 4, 5])
x: int = arr[1]
y: str = arr["2"]
```

output:

```python
$ mypy --strict foo.py
$ echo $?
0
```

> Warning:

> Based on PEP 484, the `@overload` decorator just **for type checker only**, it does not implement
> the real overloading like c++/java. Thus, we have to implement one exactly non-`@overload`
> function. At the runtime, calling the `@overload` function will raise `NotImplementedError`.

```python
from typing import List,Union,overload

classArray(object):
    def __init__(self, arr: List[int]) -> None:
        self.arr = arr

    @overload
    def __getitem__(self, i: Union[int, str]) -> Union[int, str]:
        if isinstance(i, int):
            return self.arr[i]
        if isinstance(i, str):
            return str(self.arr[int(i)])

arr = Array([1, 2, 3, 4, 5])
try:
    x: int = arr[1]
except NotImplementedError as e:
    print("NotImplementedError")
```

output:

```sh
$ python foo.py
NotImplementedError
```
