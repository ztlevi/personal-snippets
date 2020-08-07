python call by object reference. jeffknupp.com/blog/2012/11/13/is-python-callbyvalue-or-callbyreference-neither
---

# Everything in python is object
 
# 1. mutable object: list, set, dict
def foo(bar):
    bar.append(42)
    print(bar)
    # >> [42]
 
answer_list = []
foo(answer_list)
print(answer_list)
# >> [42]
 
# 2. immutable object: tuple, sting, integer, bool, float, frozenset
def foo(bar):
    bar = 'new value'
    print (bar)
    # >> 'new value'
 
answer_list = 'old value'
foo(answer_list)
print(answer_list)
# >> 'old value'