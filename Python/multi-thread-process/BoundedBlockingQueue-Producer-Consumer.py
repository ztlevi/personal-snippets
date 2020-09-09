#!/usr/bin/env python3

import collections
import random
import time
from threading import Lock, Semaphore, Thread


class BoundedBlockingQueue:
    def __init__(self, capacity: int):
        self.q = collections.deque()
        self.pushing = Semaphore(capacity)
        self.pulling = Semaphore(0)
        self.editing = (
            Lock()
        )  # This is optional because it's a queue and only append to the end

    def enqueue(self, element: int) -> None:
        self.pushing.acquire()
        self.editing.acquire()
        print(f"Enqueue: {element}")
        self.q.append(element)
        self.editing.release()
        self.pulling.release()

    def dequeue(self) -> int:
        self.pulling.acquire()
        self.editing.acquire()
        print("Deque")
        res = self.q.popleft()
        self.editing.release()
        self.pushing.release()
        return res

    def size(self) -> int:
        self.editing.acquire()
        res = len(self.q)
        self.editing.release()
        return res


class Producer(Thread):
    def __init__(
        self,
        group=None,
        target=None,
        name=None,
        bbq=None,
        args=(),
        kwargs=None,
        verbose=None,
    ):
        super(Producer, self).__init__()
        self.target = target
        self.name = name
        self.bbq = bbq

    def run(self):
        for i in range(100):
            self.bbq.enqueue(i)


class Consumer(Thread):
    def __init__(
        self,
        group=None,
        target=None,
        name=None,
        bbq=None,
        args=(),
        kwargs=None,
        verbose=None,
    ):
        super(Consumer, self).__init__()
        self.target = target
        self.name = name
        self.bbq = bbq

    def run(self):
        for i in range(100):
            res = self.bbq.dequeue()


bbq = BoundedBlockingQueue(10)
p = Producer(name="producer", bbq=bbq)
c = Consumer(name="consumer", bbq=bbq)
p.start()
c.start()
