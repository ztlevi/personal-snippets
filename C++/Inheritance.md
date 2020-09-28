# **Modes of Inheritance**

1.  **Public mode**: If we derive a sub class from a public base class. Then the public member of
    the base class will become public in the derived class and protected members of the base class
    will become protected in derived class.
2.  **Protected mode**: If we derive a sub class from a Protected base class. Then both public
    member and protected members of the base class will become protected in derived class.
3.  **Private mode**: If we derive a sub class from a Private base class. Then both public member
    and protected members of the base class will become Private in derived class.

**Note :** The private members in the base class cannot be directly accessed in the derived class,
while protected members can be directly accessed. For example, Classes B, C and D all contain the
variables x, y and z in below example. It is just question of access.

```c++
// C++ Implementation to show that a derived class
// doesn’t inherit access to private data members.
// However, it does inherit a full parent object
class A {
public:
  int x;

protected:
  int y;

private:
  int z;
};

class B : public A {
  // x is public
  // y is protected
  // z is not accessible from B
};

class C : protected A {
  // x is protected
  // y is protected
  // z is not accessible from C
};

class D : private A // 'private' is default for classes
{
  // x is private
  // y is private
  // z is not accessible from D
};
```

The below table summarizes the above three modes and shows the access specifier of the members of
base class in the sub class when derived in public, protected and private modes:

![img](https://www.geeksforgeeks.org/wp-content/uploads/table-class.png)

# Public vs Private vs Protected

The default access for members and classes is private.

```c++
class Base {
public:
   // public members go here
protected:

   // protected members go here
private:
   // private members go here
};
```

A public member is accessible from anywhere outside the class but within a program. You can set and
get the value of public variables without any member.

A private member variable or function cannot be accessed, or even viewed from outside the class.
Only the class and friend functions can access private members.

A protected member variable or function is very similar to a private member but it provided one
additional benefit that they can be accessed in child classes which are called derived classes.

# Pure Virtual Function

Sometimes implementation of all function cannot be provided in a base class because we don’t know
the implementation. Such a class is called abstract class. For example, let Shape be a base class.
We cannot provide implementation of function draw() in Shape, but we know every derived class must
have implementation of draw(). Similarly an Animal class doesn’t have implementation of move()
(assuming that all animals move), but all animals must know how to move. We cannot create objects of
abstract classes.

A pure virtual function (or abstract function) in C++ is a virtual function for which we don’t have
implementation, we only declare it. A pure virtual function is declared by assigning 0 in
declaration. See the following example.

## Some Interesting Facts:

1. A class is abstract if it has at least one pure virtual function.
2. We can have pointers and references of abstract class type.
3. If we do not override the pure virtual function in derived class, then derived class also becomes
   abstract class.
4. An abstract class can have constructors.

```c++
#include <iostream>
using namespace std;

class Base {
  int x;

public:
  virtual void fun() = 0;
  int getX() { return x; }
};

// This class inherits from Base and implements fun()
class Derived : public Base {
  int y;

public:
  void fun() { cout << "fun() called"; }
};

int main(void) {
  Derived d;
  d.fun();
  return 0;
}
```

Output:

```
fun() called
```

## **Comparison with Java**

In Java, a class can be made abstract by using abstract keyword. Similarly a function can be made
pure virtual or abstract by using abstract keyword. See  
[Abstract Classes in Java](https://www.geeksforgeeks.org/abstract-classes-in-java/) for more
details.

## **Interface vs Abstract Classes:**

An interface does not have implementation of any of its methods, it can be considered as a
collection of method declarations. In C++, an interface can be simulated by making all methods as
pure virtual. In Java, there is a separate keyword for interface.

Please write comments if you find anything incorrect, or you want to share more information about
the topic discussed above
