https://jiaweihuang.com/post/2015-11-28-atomic-vs-volatile/#:~:text=std%3A%3Aatomic%20is%20for,for%20working%20with%20special%20memory.

# For tl;dr people, from [Effective Modern C++](http://shop.oreilly.com/product/0636920033707.do):

- `std::atomic` is for data accessed from multiple threads without using mutexes. It’s a tool for writing concurrent
  software.
- `volatile` is for memory where reads and writes should not be optimized away. It’s a tool for working with special
  memory.

`volatile` is a type qualifier that you can use to declare that an object can be modified in the program by the
hardware. It has nothing to do with concurrent programming. However, some compilers, e.g.
[Microsoft C++ compiler](https://msdn.microsoft.com/en-us/library/12a04hfd%28v=vs.140%29.aspx), have imbued volatile
with semantics that render it applicable to concurrent software.

`std::atomic` template offer operations that are guaranteed to be seen as atomic by other threads. Once a std::atomic
object has been constructed, operations on it behave as if they were inside a mutex­protected critical section. The
operations are generally implemented using special machine instructions that are more efficient than would be the case
of a mutex were employed.

I did some performance tests using codelets from
[Comparing the performance of atomic, spinlock and mutex](http://demin.ws/blog/english/2012/05/05/atomic-spinlock-mutex/)
for `std::atomic` and `volatile`. The following results are from GCC 4.8.5 (Ubuntu 4.8.5­1ubuntu1~14.04), GCC 5.2.1
(Ubuntu 5.2.1­22ubuntu2), and MSVC 2013 Update 5 on an Intel Core i7­4790 CPU. Without proper locking, the increment and
decrement operations for `volatile` are not atomic and the output is **not deterministic** neither for GCC nor for
MSVC 2013. Using `std::atomic` template is around 10 times slower for GCC and 5 times slower for MSVC than `volatile`
but it guarantees the correct result and doesn’t look like there is a practical performance difference.

|                    | GCC 4.8.5 on Ubuntu 14.04 VM         | GCC 5.2.1 on Ubuntu 15.10 VM         | MSVC 2013 Update 5 on Windows 7      |
| ------------------ | ------------------------------------ | ------------------------------------ | ------------------------------------ |
| `volatile int`     | 39.3956 ms / 39.988 ms / 42.4279 ms  | 58.0358 ms / 58.5282 ms / 58.0955 ms | 100.02 ms / 109.522 ms / 106.021 ms  |
| `std::atomic<int>` | 346.683 ms / 362.175 ms / 314.465 ms | 436.179 ms / 429.875 ms / 439.644 ms | 488.098 ms / 529.106 ms / 533.607 ms |

The `volatile` keyword in C++11 ISO Standard code is to be used only for hardware access; do not use it for inter­thread
communication. For inter­thread communication, use mechanisms such as `std::atomic` from the C++ Standard Template
Library.

## Updated on March 21, 2020

Q: Why 5-10x slower isn’t a practical performance difference?  
A: I should have made it clear that the numbers in the table are from 20 million additions and 10 million subtractions
on an integer. For each increment and decrement operation, the running time is almost negligible. Therefore, 10x slower
isn’t a practical performance difference in real life applications.

For `volatile int`:

```c++
#include <chrono>
#include <future>
#include <iostream>

volatile int vi = 0;

void loop(bool inc, int max) {
    for (int i = 0; i < max; ++i) {
        if (inc) {
            ++vi;
        } else {
            --vi;
        }
    }
}

int main() {
    auto start = std::chrono::steady_clock::now();
    auto f = std::async(std::launch::async, std::bind(loop, true, 20'000'000));
    loop(false, 10'000'000);
    f.wait();
    auto end = std::chrono::steady_clock::now();

    std::cout << vi << std::endl;

    auto diff = end - start;
    auto ms = std::chrono::duration<double, std::milli>(diff).count();
    std::cout << ms << std::endl;

    return 0;
}
```

For `std::atomic<int>`:

```c++
#include <atomic>
#include <chrono>
#include <future>
#include <iostream>

std::atomic<int> ai(0);

void loop(bool inc, int max) {
    for (int i = 0; i < max; ++i) {
        if (inc) {
            ++ai;
        } else {
            --ai;
        }
    }
}

int main() {
    auto start = std::chrono::steady_clock::now();
    auto f = std::async(std::launch::async, std::bind(loop, true, 20'000'000));
    loop(false, 10'000'000);
    f.wait();
    auto end = std::chrono::steady_clock::now();

    std::cout << ai << std::endl;

    auto diff = end - start;
    auto ms = std::chrono::duration<double, std::milli>(diff).count();
    std::cout << ms << std::endl;

    return 0;
}
```

The result should be 10 million but the value is not guaranteed using `volatile int`. Of course, the testing code is
preliminary and there is room for improvement.
