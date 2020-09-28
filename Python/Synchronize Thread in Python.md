https://hackernoon.com/synchronization-primitives-in-python-564f89fee732

We’ll be learning about Locks, RLocks, Semaphores, Events, Conditions and Barriers. Of course, you
can construct your own custom synchronization primitives by subclassing these classes. We’ll start
with Locks as they are the simplest primitives and gradually we’ll move on to primitives with more
and more sophistication.

## Locks

Locks are perhaps the simplest synchronization primitives in Python. A Lock has only two
states — locked and unlocked (surprise). It is created in the unlocked state and has two principal
methods — acquire() and release(). The acquire() method locks the Lock and blocks execution until
the release() method in some other coroutine sets it to unlocked. Then it locks the Lock again and
returns True. The release() method should only be called in the locked state, it sets the state to
unlocked and returns immediately. If release() is called in the unlocked state, a RunTimeError is
raised.

Here’s the code which uses a Lock primitive for securely accessing a shared variable:

```python
#lock_tut.py
from threading import Lock, Thread
lock = Lock()
g = 0

def add_one():
   """
   Just used for demonstration. It’s bad to use the ‘global’
   statement in general.
   """

   global g
   lock.acquire()
   g += 1
   lock.release()

def add_two():
   global g
   lock.acquire()
   g += 2
   lock.release()

threads = []
for func in [add_one, add_two]:
   threads.append(Thread(target=func))
   threads[-1].start()

for thread in threads:
   """
   Waits for threads to complete before moving on with the main
   script.
   """
   thread.join()

print(g)
```

This simply gives an output of 3, but now we are sure that the two functions are not changing the
value of the global variable g simultaneously although they run on two different threads. Thus,
Locks can be used to avoid inconsistent output by allowing only one thread to modify data at a time.

## RLocks

The standard Lock doesn’t know which thread is currently holding the lock. If the lock is held, any
thread that attempts to acquire it will block, even if the same thread itself is already holding the
lock. In such cases, RLock (re-entrant lock) is used. You can extend the code in the following
snippet by adding output statements for demonstrating how RLocks can prevent unwanted blocking.

```python
#rlock_tut.py
import threading

num = 0
lock = Threading.Lock()

lock.acquire()
num += 1
lock.acquire() # This will block.
num += 2
lock.release()


# With RLock, that problem doesn’t happen.
lock = Threading.RLock()

lock.acquire()
num += 3
lock.acquire() # This won’t block.
num += 4
lock.release()
lock.release() # You need to call release once for each call to acquire.
```

One good use case for RLocks is recursion, when a parent call of a function would otherwise block
its nested call. Thus, the main use for RLocks is nested access to shared resources.

## Semaphores

Semaphores are simply advanced counters. An acquire() call to a semaphore will block only after a
number of threads have acquire()ed it. The associated counter decreases per acquire() call, and
increases per release() call. A ValueError will occur if release() calls try to increment the
counter beyond it’s assigned maximum value (which is the number of threads that can acquire() the
semaphore before blocking occurs). Following code demonstrates the use of semaphores in a simple
producer-consumer problem.

```python
#semaphores_tut.py
import random, time
from threading import BoundedSemaphore, Thread
max_items = 5
"""
Consider 'container' as a container, of course, with a capacity of 5
items. Defaults to 1 item if 'max_items' is passed.
"""
container = BoundedSemaphore(max_items)
def producer(nloops):
    for i in range(nloops):
        time.sleep(random.randrange(2, 5))
        print(time.ctime(), end=": ")
        try:
            container.release()
            print("Produced an item.")
        except ValueError:
            print("Full, skipping.")
def consumer(nloops):
    for i in range(nloops):
        time.sleep(random.randrange(2, 5))
        print(time.ctime(), end=": ")
        """
        In the following if statement we disable the default
        blocking behaviour by passing False for the blocking flag.
        """
        if container.acquire(False):
            print("Consumed an item.")
        else:
            print("Empty, skipping.")
threads = []
nloops = random.randrange(3, 6)
print("Starting with %s items." % max_items)
threads.append(Thread(target=producer, args=(nloops,)))
threads.append(Thread(target=consumer, args=(random.randrange(nloops, nloops+max_items+2),)))
for thread in threads:  # Starts all the threads.
    thread.start()
for thread in threads:  # Waits for threads to complete before moving on with the main script.
    thread.join()
print("All done.")
```

The threading module also provides the simple Semaphore class. A Semaphore provides a non-bounded
counter which allows you to call release() any number of times for incrementing. However, to avoid
programming errors, it’s usually a correct choice to use BoundedSemaphore , which raises an error if
a release() call tries to increase the counter beyond it’s maximum size.

Semaphores are typically used for limiting a resource, like limiting a server to handle only 10
clients at a time. In such a case, multiple thread connections compete for a limited resource (in
our example, it is the server).

## Events

The Event synchronization primitive acts as a simple communicator between threads. They are based on
an internal flag which threads can set() or clear(). Other threads can wait() for the internal flag
to be set(). The wait() method blocks until the flag becomes true. Following snippet demonstrates
how Events can be used to trigger actions.

```python
#event_tut.py
import random, time
from threading import Event, Thread

event = Event()

def waiter(event, nloops):
    for i in range(nloops):
    print(“%s. Waiting for the flag to be set.” % (i+1))
    event.wait() # Blocks until the flag becomes true.
    print(“Wait complete at:”, time.ctime())
    event.clear() # Resets the flag.
    print()

def setter(event, nloops):
    for i in range(nloops):
    time.sleep(random.randrange(2, 5)) # Sleeps for some time.
    event.set()

threads = []
nloops = random.randrange(3, 6)

threads.append(Thread(target=waiter, args=(event, nloops)))
threads[-1].start()
threads.append(Thread(target=setter, args=(event, nloops)))
threads[-1].start()

for thread in threads:
    thread.join()

print(“All done.”)
```

## Conditions

A Condition object is simply a more advanced version of the Event object. It too acts as a
communicator between threads and can be used to notify() other threads about a change in the state
of the program. For example, it can be used to signal the availability of a resource for
consumption. Other threads must also acquire() the condition (and thus its related lock) before
wait()ing for the condition to be satisfied. Also, a thread should release() a Condition once it has
completed the related actions, so that other threads can acquire the condition for their purposes.
Following code demonstrates the implementation of another simple producer-consumer problem with the
help of the Condition object.

```python
#condition_tut.py
import random, time
from threading import Condition, Thread
"""
'condition' variable will be used to represent the availability of a produced
item.
"""
condition = Condition()
box = []
def producer(box, nitems):
    for i in range(nitems):
        time.sleep(random.randrange(2, 5))  # Sleeps for some time.
        condition.acquire()
        num = random.randint(1, 10)
        box.append(num)  # Puts an item into box for consumption.
        condition.notify()  # Notifies the consumer about the availability.
        print("Produced:", num)
        condition.release()
def consumer(box, nitems):
    for i in range(nitems):
        condition.acquire()
        condition.wait()  # Blocks until an item is available for consumption.
        print("%s: Acquired: %s" % (time.ctime(), box.pop()))
        condition.release()
threads = []
"""
'nloops' is the number of times an item will be produced and
consumed.
"""
nloops = random.randrange(3, 6)
for func in [producer, consumer]:
    threads.append(Thread(target=func, args=(box, nloops)))
    threads[-1].start()  # Starts the thread.
for thread in threads:
    """Waits for the threads to complete before moving on
       with the main script.
    """
    thread.join()
print("All done.")
```

There can be other uses of Conditions. I think they will be useful when you’re developing a
streaming API which notifies a waiting client once a piece of data is available.

## Barriers

A barrier is a simple synchronization primitive which can be used by different threads to wait for
each other. Each thread tries to pass a barrier by calling the wait() method, which will block until
all of threads have made that call. As soon as that happens, the threads are released
simultaneously. Following snippet demonstrates the use of Barriers.

```python
#barrier_tut.py
from random import randrange
from threading import Barrier, Thread
from time import ctime, sleep

num = 4
# 4 threads will need to pass this barrier to get released.
b = Barrier(num)
names = [“Harsh”, “Lokesh”, “George”, “Iqbal”]

def player():
    name = names.pop()
    sleep(randrange(2, 5))
    print(“%s reached the barrier at: %s” % (name, ctime()))
    b.wait()

threads = []
print(“Race starts now…”)

for i in range(num):
    threads.append(Thread(target=player))
    threads[-1].start()
"""
Following loop enables waiting for the threads to complete before moving on with the main script.
"""
for thread in threads:
    thread.join()
print()
print(“Race over!”)
```

Barriers can find many uses; one of them being synchronizing a server and a client — as the server
has to wait for the client after initializing itself.

With that, we have reached the end of our discussion on synchronization primitives in Python. I
wrote this post as a solution to an exercise in the book “Core Python Applications Programming” by
Wesley Chun. If you liked this post, consider having a look at my other works from this book on
GitHub and starring the repository 🙂. The gists for code mentioned in this article are also
available at my profile.
