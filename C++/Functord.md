# Functor

https://www.quantstart.com/articles/Function-Objects-Functors-in-C-Part-1

This is the first in a series of articles on how to make use of function objects in C++. Many of the concepts within
quantitative finance are represented by functions or groups of functions. The types of these functions can vary
considerably. Not all of the functions we consider are real-valued, for instance. Many functions take other functions as
argument or are vector-valued.

Our goal in this introductory article is to develop some intuition about basic mathematical functions and attempt to
model their behaviour in C++. Because C++ is a "federation of languages" and supports multi-paradigm programming, there
are many options available to us. There is no "right answer" as to how to model functions in C++, rather there exist
different methods, some of which are more optimal in certain situations.

There are plenty of examples of functions from within mathematics, and quantitative finance in particular. Some concrete
examples from quantitative finance include:

- **Pay-off functions** - These take a real-valued asset spot and strike price, generally, to provide a real-valued
  option value at expiry.
- **Differential Equation Coefficients** - ODEs and PDEs possess coefficients dependent upon various parameters, which
  can potentially be real-valued functions.
- **Matrices** - Linear Algebra tells us that matrices are in fact linear maps, i.e. functions between vector spaces.

We will strike a balance between re-use, efficiency and maintainability when modelling functions in C++, so as to
improve productivity without creating complicated code. C++ presents us with a number of alternatives for modelling
functions. In particular:

- **Function Pointers** - These are a feature of the C language and so form part of the C++ standard. A function pointer
  allows a pointer to a function to be passed as a parameter to another function.
- **Function Objects (Functors)** - C++ allows the function call `operator()` to be overloaded, such that an object
  instantiated from a class can be "called" like a function.
- **STL Functions** - The Standard Template Library (STL) provides three types of _template function objects_:
  Generator, unary and binary functions.
- **C++11** `<function>` - C++11 brought new changes to how functors were handled. In addition, anonymous functions
  (lambdas) are now supported.

## Function Pointers

Function pointers are a legacy feature from the C language. C++ is a superset of C and so includes function pointer
syntax. In essence, function pointers point to executable code at a particular piece of memory, rather than a data value
as with other pointers. Dereferencing the function pointer allows the code in the memory block to be executed. In
addition, arguments can be passed to the function pointer, which are then passed to the executed code block. The main
benefit of function pointers is that they provide a straightforward mechanism for choosing a function to execute at
run-time.

Here is a C++ example which makes use of an `add` function and a `multiply` function to sum and multiply two double
values:

```c++
#include <iostream>

double add(double left, double right) {
    return left + right;
}

double multiply(double left, double right) {
    return left * right;
}

double binary_op(double left, double right, double (*f)(double, double)) {
    return (*f)(left, right);
}

int main( ) {
    double a = 5.0;
    double b = 10.0;

    std::cout << "Add: " << binary_op(a, b, add) << std::endl;
    std::cout << "Multiply: " << binary_op(a, b, multiply) << std::endl;

    return 0;
}
```

The output from this simple example is as follows:

<pre class="prettyprint">
Add: 15
Multiply: 50
</pre>

In order for a function to receive a function pointer as a parameter it is necessary to specify its return type (in this
case `double`), the parameter name of the function (in this case `f`) and the types of all parameters necessary for the
function pointer (in this case two `double` values). We are then able to pass the function pointers into the `binary_op`
function. `binary_op` then dereferences the function pointer in order to execute the correct function - `add` or
`multiply`.

While function pointers are simple to use in your code, they do suffer from some significant drawbacks:

- **Efficiency** - Function pointers are inefficient when compared with _functors_ (discussed below). The compiler will
  often pass them as raw pointers and as such the compiler will struggle to inline the code.
- **State** - Function pointers by themselves are not particularly flexible at storing _state_. Although it is possible,
  by using a local `static` variable within the function, there is only ever one global state for the function itself
  and as such this static variable must be shared. Furthermore this static variable will not be _thread-safe_, unless
  the appropriate thread synchronisation code is added. Thus it can lead to bottlenecks or even race conditions in
  multithreaded programs.
- **Templates** - Function pointers do not play too well with templates if there are multiple signatures of the function
  in your code. The solution is to use function pointer casting, which leads to difficult and ungainly syntax.
- **Adaptation** - Function pointers have fixed parameter types and quantities. Thus they are not particularly flexible
  when external functions with differing parameter types could be used. Although adapting the function pointers (by
  wrapping the external functions with hard-coded parameters) is possible, it leads to poor flexibility and bloated
  code.

The solution to these problems is to make use of the C++ _function object_ (also known as a _functor_).

## C++ Function Objects (Functors)

A function object allows an instance object of a class to be called or invoked as if it were an ordinary function. In
C++ this is carried out by overloading `operator()`. The main benefit of using function objects is that _they are
objects_ and hence can contain state, either statically across all instances of the function objects or individually on
a particular instance.

Here is a C++ example of a function object (in fact a function object hierarchy), which replaces the function pointer
syntax from the version above, with functors:

```c++
#include <iostream>

// Abstract base class
class BinaryFunction {
public:
  BinaryFunction() {};
  virtual double operator() (double left, double right) = 0;
};

// Add two doubles
class Add : public BinaryFunction {
public:
  Add() {};
  virtual double operator() (double left, double right) { return left+right; }
};

// Multiply two doubles
class Multiply : public BinaryFunction {
public:
  Multiply() {};
  virtual double operator() (double left, double right) { return left*right; }
};

double binary_op(double left, double right, BinaryFunction* bin_func) {
  return (*bin_func)(left, right);
}

int main( ) {
  double a = 5.0;
  double b = 10.0;

  BinaryFunction* pAdd = new Add();
  BinaryFunction* pMultiply = new Multiply();

  std::cout << "Add: " << binary_op(a, b, pAdd) << std::endl;
  std::cout << "Multiply: " << binary_op(a, b, pMultiply) << std::endl;

  delete pAdd;
  delete pMultiply;

  return 0;
}
```

Firstly, note that there is a lot more happening in the code! We have created an _abstract base class_, called
`BinaryFunction` and then inherited `Add` and `Multiply` classes. Since `BinaryFunction` is abstract, it cannot be
instantiated. Hence we need to make use of pointers to pass in `pAdd` and `pMultiply` to the new `binary_op` function.

An obvious question is "What do we gain from all this extra code/syntax?". The main benefit is that we are now able to
add _state_ to the function objects. For `Add` and `Multiply` this is likely be to unnecessary. However, if our
inheritance hierarchy were modelling connections to a database, then we might require information about how many
connections currently exist (as we wouldn't want to open too many for performance reasons). We might also want to add
extra database types. An inheritance hierarchy like this allows us to easily create more database connection classes
without modifying any other code.

We have already seen a good use of function objects when we implemented PayOff classes in other articles, such as with
[Asian options pricers via Monte Carlo](http://quantstart.com/articles/Asian-option-pricing-with-C-via-Monte-Carlo-Methods).

In the next article on function objects we will consider STL Functions, the Boost library for function objects and the
new changes for C++11 functors.
