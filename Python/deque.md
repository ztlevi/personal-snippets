## https://docs.python.org/3/library/collections.html#collections.deque

class collections.deque([iterable[, maxlen]]) Returns a new deque object initialized left-to-right (using append()) with
data from iterable. If iterable is not specified, the new deque is empty.

Deques are a generalization of stacks and queues (the name is pronounced “deck” and is short for “double-ended queue”).
Deques support thread-safe, memory efficient appends and pops from either side of the deque with approximately the same
O(1) performance in either direction.

Though list objects support similar operations, they are optimized for fast fixed-length operations and incur O(n)
memory movement costs for pop(0) and insert(0, v) operations which change both the size and position of the underlying
data representation.
