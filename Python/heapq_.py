# According to the example from the documentation, you can use tuples, and it will sort by the first element of the tuple:

import heapq
from heapq import heappush, heappop

h = []
heappush(h, (5, "write code"))
heappush(h, (7, "release product"))
heappush(h, (1, "write spec"))
heappush(h, (3, "create tests"))
heappop(h)
# (1, 'write spec')

# heapq sorts objects the same way list.sort does, so just define a method __cmp__() within your class definition, which will compare itself to another instance of the same class:


def __cmp__(self, other):
    return cmp(self.intAttribute, other.intAttribute)


# Works in Python 2.x.

# In 3.x use:


def __lt__(self, other):
    return self.intAttribute < other.intAttribute


# Max heap
arr = [1, 2, 3, 4, 5]
heapq._heapify_max(arr)
heapq._heappop_max(arr)
heapq._heapreplace_max(arr, 6)
