# Rule of Five

The rule of three (also known as the Big Three) was a rule of thumb in c++(until c++11).Which are/was helpful to create
exception free code. After cpp11, two more rule added and it become rule of five. Which are related to following c++
methods:

- [destructor](<https://en.wikipedia.org/wiki/Destructor_(computer_programming)>)
- [copy constructor](https://en.wikipedia.org/wiki/Copy_constructor)
- [move constructor](https://en.wikipedia.org/wiki/Move_constructor)
- [copy assignment operator](https://en.wikipedia.org/wiki/Copy_assignment_operator) (after cpp11)
- [move assignment operator](https://en.wikipedia.org/wiki/Move_assignment_operator) (after cpp11)

By default compiler generated version of up above methods works until programmer has not defined any of them. If
programmer has implement any of them, he/she should implement rest of them to avoid possible(not always) exceptions.

Compilers implicitly-generated contractor, destructor and operators simply provide shallow copy. Mostly programmer
implement these methods to change/avoid this specific feature. If default behaviour is intended to be shallow copy,
there is no need of implement any of them. But if intended behaviour is deep copy(or something else), it need to be
implemented in all above operator and constructor to avoid nondeterministic nature of class.

Here is an example of rule of five based class:

```c++
#include <cstring>
#include <iostream>
#include <string>
class CPPR5Class {
public:
  // Default constructor
  CPPR5Class() : data(new char[14]) { std::strcpy(data, "Hello, World!"); }

  // Copy constructor
  CPPR5Class(const CPPR5Class &original)
      : data(new char[std::strlen(original.data) + 1]) {
    std::strcpy(data, original.data);
  }

  // Move constructor
  CPPR5Class(CPPR5Class &&original) noexcept
      : /* noexcept needed to enable optimizations in containers */
        data(original.data) {
    original.data = nullptr;
  }

  // Destructor
  ~CPPR5Class() noexcept /* explicitly specified destructors should be annotated
                            noexcept as best-practice */
  {
    delete[] data;
  }

  // Copy assignment operator
  CPPR5Class &operator=(const CPPR5Class &original) {
    CPPR5Class tmp(original); // re-use copy-constructor
    *this = std::move(tmp);   // re-use move-assignment
    return *this;
  }

  // Move assignment operator
  CPPR5Class &operator=(CPPR5Class &&original) noexcept {
    if (this == &original) {
      // take precautions against `CPPR5Class = std::move(CPPR5Class)`
      return *this;
    }
    delete[] data;
    data = original.data;
    original.data = nullptr;
    return *this;
  }

private:
  friend std::ostream &operator<<(std::ostream &os,
                                  const CPPR5Class &cPPR5Class) {
    os << cPPR5Class.data;
    return os;
  }

  char *data;
};

int main() {
  const CPPR5Class cPPR5Class;
  std::cout << cPPR5Class << std::endl;

  return 0;
}
```
