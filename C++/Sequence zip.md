# ZIP

Warning: boost::zip_iterator and boost::combine as of Boost 1.63.0 (2016 Dec 26) will cause undefined behavior if the
length of the input containers are not the same (it may crash or iterate beyond the end).

Starting from Boost 1.56.0 (2014 Aug 7) you could use boost::combine (the function exists in earlier versions but
undocumented):

```c++
#include <boost/range/combine.hpp>
#include <vector>
#include <list>
#include <string>

int main() {
    std::vector<int> a {4, 5, 6};
    double b[] = {7, 8, 9};
    std::list<std::string> c {"a", "b", "c"};
    for (auto tup : boost::combine(a, b, c, a)) {    // <---
        int x, w;
        double y;
        std::string z;
        boost::tie(x, y, z, w) = tup;
        printf("%d %g %s %d\n", x, y, z.c_str(), w);
    }
}
```

This would print

```
4 7 a 4
5 8 b 5
6 9 c 6
```
