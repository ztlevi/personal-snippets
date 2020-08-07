# Lambda

## Captures: https://en.cppreference.com/w/cpp/language/lambda

If the lambda-expression captures anything by copy (either implicitly with capture clause `**[=]**` or explicitly with a
capture that does not include the character &, e.g. `**[a, b, c]**`), the closure type includes unnamed non-static data
members, declared in unspecified order, that hold copies of all entities that were so captured.

Those data members that correspond to captures without initializers are
[direct-initialized](https://en.cppreference.com/w/cpp/language/direct_initialization "cpp/language/direct initialization")
when the lambda-expression is evaluated. Those that correspond to captures with initializers are initialized as the
initializer requires (could be copy- or direct-initialization). If an array is captured, array elements are
direct-initialized in increasing index order. The order in which the data members are initialized is the order in which
they are declared (which is unspecified).

The type of each data member is the type of the corresponding captured entity, except if the entity has reference type
(in that case, references to functions are captured as lvalue references to the referenced functions, and references to
objects are captured as copies of the referenced objects).

For the entities that are captured by reference (with the default capture `**[&]**` or when using the character &, e.g.
`**[&a, &b, &c]**`), it is unspecified if additional data members are declared in the closure type , but any such
additional members must satisfy
[LiteralType](https://en.cppreference.com/w/cpp/named_req/LiteralType "cpp/named req/LiteralType") (since C++17).

### Lambda Capture

The captures is a comma-separated list of zero or more captures, optionally beginning with the capture-default. The only
capture defaults are

- **&** (implicitly capture the used automatic variables by reference) and
- **=** (implicitly capture the used automatic variables by copy).

## Examples

- `sort`

  ```cpp
  std::sort(vecVals.begin(), vecVals.end(),
      [](int x, int y){return x < y;});
  ```

- `copy_if`

  ```cpp
  std::copy_if(vecVals.begin(), vecVals.end(), std::back_inserter(evenVecVals),
      [](int x) { return (x % 2) == 0; });
  ```

- `for_each`:

  ```cpp
    std::for_each(vecVals.begin(), vecVals.end(),
                [&](int x) { doubleVec.push_back(x * 2); });
  ```

- `transform`

```cpp
  transform(vec1.begin(), vec1.end(), vec2.begin(), vec3.begin(),
            [](int x, int y) { return x + y; });
```

- `std::function`

```cpp
std::function<int(int)> Fib = [&Fib](int n) {
    return n < 2 ? 1 : Fib(n - 1) + Fib(n - 2);
  };
```

- `priority_queue`

```cpp
auto cmp = [](int x, int y)->bool{return x<y;};
std::priority_queue<int, std::vector<int>, decltype(cmp)> PQ(cmp);
```
