# Print Foobar alternately

## Desccription

Suppose you are given the following code:

```python
class FooBar {
  public void foo() {
    for (int i = 0; i < n; i++) {
      print("foo");
    }
  }

  public void bar() {
    for (int i = 0; i < n; i++) {
      print("bar");
    }
  }
}
```

The same instance of `FooBar` will be passed to two different threads. Thread A will call `foo()` while thread B will
call `bar()`. Modify the given program to output "foobar" _n_ times.

**Example 1:**

```python
**Input:** n = 1
**Output:** "foobar"
**Explanation:** There are two threads being fired asynchronously. One of them calls foo(), while the other calls bar(). "foobar" is being output 1 time.
```

**Example 2:**

```python
**Input:** n = 2
**Output:** "foobarfoobar"
**Explanation:** "foobar" is being output 2 times.
```

## Semaphore Solution

Use two Semaphores just as we used two locks. The `foo_gate` semaphore starts with a value of 1 because we want `foo` to
print first. `Semaphore(int num)` num means the initial permit count.

```
Semaphore(value: int=...)
This class implements semaphore objects.

Semaphores manage a counter representing the number of release() calls minus
the number of acquire() calls, plus an initial value. The acquire() method
blocks if necessary until it can return without making the counter
negative. If not given, value defaults to 1.
```

```python
import threading
from threading import Semaphore


class FooBar:
    def __init__(self, n):
        self.n = n
        self.foo_gate = Semaphore(1)
        self.bar_gate = Semaphore(0)

    def foo(self, printFoo):
        for i in range(self.n):
            self.foo_gate.acquire()
            printFoo()
            print(f"foo:{i}")
            self.bar_gate.release()

    def bar(self, printBar):
        for i in range(self.n):
            self.bar_gate.acquire()
            printBar()
            print(f"bar:{i}")
            self.foo_gate.release()


foobar = FooBar(100)


def printFoo():
    print("Foo")


def printBar():
    print("Bar")


t1 = threading.Thread(target=foobar.foo, args=(printFoo,))
t2 = threading.Thread(target=foobar.bar, args=(printBar,))
t1.start()
t2.start()
```

## Barrier Solution

Raise a barrier which makes both threads wait for each other before they are allowed to continue. `foo` prints before
reaching the barrier. `bar` prints after reaching the barrier.

```
Barrier(parties: int, action: Optional[Callable[[], None]]=..., timeout: Optional[float]=...)
Implements a Barrier.

Useful for synchronizing a fixed number of threads at known synchronization
points.  Threads block on 'wait()' and are simultaneously awoken once they
have all made that call.
```

```python
import threading
from threading import Barrier


class FooBar:
    def __init__(self, n):
        self.n = n
        self.barrier = Barrier(3)

    def foo(self, printFoo):
        for i in range(self.n):
            printFoo()
            self.barrier.wait()

    def bar(self, printBar):
        for i in range(self.n):
            self.barrier.wait()
            printBar()

    def boo(self, printBoo):
        for i in range(self.n):
            self.barrier.wait()
            printBoo()


foobar = FooBar(100)


def printFoo():
    print("Foo")


def printBar():
    print("Bar")


def printBoo():
    print("Boo")


t1 = threading.Thread(target=foobar.foo, args=(printFoo,))
t2 = threading.Thread(target=foobar.bar, args=(printBar,))
t3 = threading.Thread(target=foobar.boo, args=(printBoo,))
t1.start()
t2.start()
t3.start()
```

## Condition Variable Solution

Count the number of times foo and bar was printed and only print foo if the number of times is equal. `bar` prints if
foo was printed fewer times. Use Condition and `wait_for` to syncrhonize the threads.

```
Condition(lock: Union[Lock, _RLock, None]=...)
Class that implements a condition variable.

A condition variable allows one or more threads to wait until they are
notified by another thread.

If the lock argument is given and not None, it must be a Lock or RLock
object, and it is used as the underlying lock. Otherwise, a new RLock object
is created and used as the underlying lock.
```

```python
from threading import Condition
class FooBar:
    def __init__(self, n):
        self.n = n
        self.foo_counter = 0
        self.bar_counter = 0
        self.condition = Condition()

    def foo(self, printFoo):
        for i in range(self.n):
            with self.condition:
                self.condition.wait_for(lambda: self.foo_counter == self.bar_counter)
                printFoo()
                self.foo_counter += 1
                self.condition.notify(1)

    def bar(self, printBar):
        for i in range(self.n):
            with self.condition:
                self.condition.wait_for(lambda: self.foo_counter > self.bar_counter)
                printBar()
                self.bar_counter += 1
                self.condition.notify(1)
```

```
from threading import Condition
class FooBar:
    def __init__(self, n):
        self.n = n
        self.foo_counter = 0
        self.bar_counter = 0
        self.condition = Condition()

    def foo(self, printFoo):
        for i in range(self.n):
            with self.condition:
                if self.foo_counter > self.bar_counter:
                    self.condition.wait()
                printFoo()
                self.foo_counter += 1
                self.condition.notify(1)

    def bar(self, printBar):
        for i in range(self.n):
            with self.condition:
                if self.foo_counter == self.bar_counter:
                    self.condition.wait()
                printBar()
                self.bar_counter += 1
                self.condition.notify(1)
```

## Event Solution

Each thread can wait on each other to set their corresponding `foo_printed` and `bar_printed` events. Each thread also
resets the corresponding printed events with `.clear()` for the next loop iteration.

```
Event()
Class implementing event objects.

Events manage a flag that can be set to true with the set() method and reset
to false with the clear() method. The wait() method blocks until the flag is
true.  The flag is initially false.
```

```python
from threading import Event

class FooBar:
    def __init__(self, n):
        self.n = n
        self.foo_printed = Event()
        self.bar_printed = Event()
        self.bar_printed.set()

    def foo(self, printFoo):
        for i in range(self.n):
            self.bar_printed.wait()
            self.bar_printed.clear()
            printFoo()
            self.foo_printed.set()

    def bar(self, printBar):
        for i in range(self.n):
            self.foo_printed.wait()
            self.foo_printed.clear()
            printBar()
            self.bar_printed.set()
```

## Lock Solution

Use two locks for the threads to signal to each other when the other should run. `bar_lock` starts in a locked state
because we always want `foo` to print first.

```python
from threading import Lock

class FooBar:
    def __init__(self, n):
        self.n = n
        self.foo_lock = Lock()
        self.bar_lock = Lock()
        self.bar_lock.acquire()

    def foo(self, printFoo):
        for i in range(self.n):
            self.foo_lock.acquire()
            printFoo()
            self.bar_lock.release()

	def bar(self, printBar):
        for i in range(self.n):
            self.bar_lock.acquire()
            printBar()
            self.foo_lock.release()
```
