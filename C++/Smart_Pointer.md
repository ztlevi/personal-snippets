# Smart Pointer

```c++
#include<iostream>
using namespace std;

// A generic smart pointer class
template <class T>
class SmartPtr
{
T *ptr; // Actual pointer
public:
// Constructor
explicit SmartPtr(T *p = NULL) { ptr = p; }

// Destructor
~SmartPtr() { delete(ptr); }

// Overloading dereferncing operator
T & operator * () { return *ptr; }

// Overloding arrow operator so that members of T can be accessed
// like a pointer (useful if T represents a class or struct or
// union type)
T * operator -> () { return ptr; }
};

int main()
{
	SmartPtr<int> ptr(new int());
	*ptr = 20;
	cout << *ptr;
	return 0;
}
```

## Unique Pointer

The following example shows how to create unique_ptr instances and pass them between functions.

```c++
unique_ptr<Song> SongFactory(const std::wstring &artist,
                             const std::wstring &title) {
  // Implicit move operation into the variable that stores the result.
  return make_unique<Song>(artist, title);
}

void MakeSongs() {
  // Create a new unique_ptr with a new object.
  auto song = make_unique<Song>(L"Mr. Children", L"Namonaki Uta");

  // Use the unique_ptr.
  vector<wstring> titles = {song->title};

  // Move raw pointer from one unique_ptr to another.
  unique_ptr<Song> song2 = std::move(song);

  // Obtain unique_ptr from function that returns by value.
  auto song3 = SongFactory(L"Michael Jackson", L"Beat It");
}
```

The following example shows how to initialize a unique_ptr that is a class member.

```c++
class MyClass {
private:
  // MyClass owns the unique_ptr.
  unique_ptr<ClassFactory> factory;

public:
  // Initialize by using make_unique with ClassFactory default constructor.
  MyClass() : factory(make_unique<ClassFactory>()) {}

  void MakeClass() { factory->DoSomething(); }
};
```

## Shared pointer

https://docs.microsoft.com/en-us/cpp/cpp/how-to-create-and-use-shared-ptr-instances?view=vs-2019

The `shared_ptr` type is a smart pointer in the C++ standard library that is designed for scenarios in which more than
one owner might have to manage the lifetime of the object in memory. After you initialize a `shared_ptr` you can copy
it, pass it by value in function arguments, and assign it to other `shared_ptr` instances. All the instances point to
the same object, and share access to one "control block" that **increments and decrements the reference count** whenever
a new `shared_ptr` is added, goes out of scope, or is reset. When the reference count reaches zero, the control block
deletes the memory resource and itself.

```c++

// Use make_shared function when possible.
auto sp1 = make_shared<Song>(L"The Beatles", L"Im Happy Just to Dance With You");

// Ok, but slightly less efficient.
// Note: Using new expression as constructor argument
// creates no named variable for other code to access.
shared_ptr<Song> sp2(new Song(L"Lady Gaga", L"Just Dance"));

// When initialization must be separate from declaration, e.g. class members,
// initialize with nullptr to make your programming intent explicit.
shared_ptr<Song> sp5(nullptr);
//Equivalent to: shared_ptr<Song> sp5;
//...
sp5 = make_shared<Song>(L"Elton John", L"I'm Still Standing");
```

### Shared_pointer c++ implementation

```c++
#include <iostream>

template <class T> class SharedPtr {
  T *sharedPtr;
  mutable int *refCount;

public:
  SharedPtr(T *ptr) {
    sharedPtr = ptr;
    refCount = new int();
    *refCount = 1;
  }
  ~SharedPtr() {
    std::cout << "inside destructor refCount = " << *refCount << "\n";
    if (--(*refCount) == 0)
      delete sharedPtr;
  }
  T &operator=(const SharedPtr &ptr);
  SharedPtr(const SharedPtr &ptr);
  T *operator->() { return sharedPtr; }
  T &operator*() { return *sharedPtr; }
};

template <class T> T &SharedPtr<T>::operator=(const SharedPtr &ptr) {
  std::cout << "inside assignment operator\n";
  this->sharedPtr = ptr.sharedPtr;
  (*(ptr.refCount))++;
  this->refCount = ptr.refCount;
  return *this;
}

template <class T> SharedPtr<T>::SharedPtr(const SharedPtr &ptr) {
  std::cout << "inside copy constructor\n";
  this->sharedPtr = ptr.sharedPtr;
  (*(ptr.refCount))++;
  this->refCount = ptr.refCount;
}

int main(int argc, char *argv[]) {
  SharedPtr<int> p = new int();
  { SharedPtr<int> q = p; }
  return 0;
}
```

## Weak pointer

https://docs.microsoft.com/en-us/cpp/cpp/how-to-create-and-use-weak-ptr-instances?view=vs-2019

Sometimes an object must store a way to access the underlying object of a `shared_ptr` without causing the reference
count to be incremented. Typically, this situation occurs when you have cyclic references between `shared_ptr`
instances.

The best design is to avoid shared ownership of pointers whenever you can. However, if you must have shared ownership of
`shared_ptr` instances, avoid cyclic references between them. When cyclic references are unavoidable, or even preferable
for some reason, use `weak_ptr` to give one or more of the owners a weak reference to another `shared_ptr`. By using a
`weak_ptr`, you can create a `shared_ptr` that joins to an existing set of related instances, but only if the underlying
memory resource is still valid. A `weak_ptr` itself does not participate in the reference counting, and therefore, it
cannot prevent the reference count from going to zero. However, you can use a `weak_ptr` to try to obtain a new copy of
the `shared_ptr` with which it was initialized. If the memory has already been deleted, a `bad_weak_ptr` exception is
thrown. If the memory is still valid, the new shared pointer increments the reference count and guarantees that the
memory will be valid as long as the `shared_ptr` variable stays in scope.

### Example[](https://docs.microsoft.com/en-us/cpp/cpp/how-to-create-and-use-weak-ptr-instances?view=vs-2019#example)

The following code example shows a case where `weak_ptr` is used to ensure proper deletion of objects that have circular
dependencies. As you examine the example, assume that it was created only after alternative solutions were considered.
The `Controller` objects represent some aspect of a machine process, and they operate independently. Each controller
must be able to query the status of the other controllers at any time, and each one contains a private
`vector<weak_ptr<Controller>>` for this purpose. Each vector contains a circular reference, and therefore, `weak_ptr`
instances are used instead of `shared_ptr`.

```c++
#include <iostream>
#include <memory>
#include <string>
#include <vector>
#include <algorithm>

using namespace std;

class Controller
{
public:
    int Num;
    wstring Status;
    vector<weak_ptr<Controller>> others;
    explicit Controller(int i) : Num(i) , Status(L"On")
    {
        wcout << L"Creating Controller" << Num << endl;
    }

    ~Controller()
    {
        wcout << L"Destroying Controller" << Num << endl;
    }

    // Demonstrates how to test whether the
    // pointed-to memory still exists or not.
    void CheckStatuses() const
    {
        for_each(others.begin(), others.end(), [] (weak_ptr<Controller> wp)
        {
            try
            {
                auto p = wp.lock();
                wcout << L"Status of " << p->Num << " = " << p->Status << endl;
            }

            catch (bad_weak_ptr b)
            {
                wcout << L"Null object" << endl;
            }
        });
    }
};

void RunTest()
{
    vector<shared_ptr<Controller>> v {
      make_shared<Controller>(0),
      make_shared<Controller>(1),
      make_shared<Controller>(2),
      make_shared<Controller>(3),
      make_shared<Controller>(4),
    };

    // Each controller depends on all others not being deleted.
    // Give each controller a pointer to all the others.
    for (int i = 0 ; i < v.size(); ++i)
    {
        for_each(v.begin(), v.end(), [&v,i] (shared_ptr<Controller> p)
        {
            if(p->Num != i)
            {
                v[i]->others.push_back(weak_ptr<Controller>(p));
                wcout << L"push_back to v[" << i << "]: " << p->Num << endl;
            }
        });
    }

    for_each(v.begin(), v.end(), [](shared_ptr<Controller>& p)
    {
        wcout << L"use_count = " << p.use_count() << endl;
        p->CheckStatuses();
    });
}

int main()
{
    RunTest();
    wcout << L"Press any key" << endl;
    char ch;
    cin.getline(&ch, 1);
}
```

Output:

```
Creating Controller0
Creating Controller1
Creating Controller2
Creating Controller3
Creating Controller4
push_back to v[0]: 1
push_back to v[0]: 2
push_back to v[0]: 3
push_back to v[0]: 4
push_back to v[1]: 0
push_back to v[1]: 2
push_back to v[1]: 3
push_back to v[1]: 4
push_back to v[2]: 0
push_back to v[2]: 1
push_back to v[2]: 3
push_back to v[2]: 4
push_back to v[3]: 0
push_back to v[3]: 1
push_back to v[3]: 2
push_back to v[3]: 4
push_back to v[4]: 0
push_back to v[4]: 1
push_back to v[4]: 2
push_back to v[4]: 3
use_count = 1
Status of 1 = On
Status of 2 = On
Status of 3 = On
Status of 4 = On
use_count = 1
Status of 0 = On
Status of 2 = On
Status of 3 = On
Status of 4 = On
use_count = 1
Status of 0 = On
Status of 1 = On
Status of 3 = On
Status of 4 = On
use_count = 1
Status of 0 = O
nStatus of 1 = On
Status of 2 = On
Status of 4 = On
use_count = 1
Status of 0 = On
Status of 1 = On
Status of 2 = On
Status of 3 = On
Destroying Controller0
Destroying Controller1
Destroying Controller2
Destroying Controller3
Destroying Controller4
Press any key
```
