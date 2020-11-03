# realpython.com/blog/python/primer-on-python-decorators
# 1. basic decorator description.
# 2. functools.warps
# ---

# functools.wraps
def decorator1(my_func):
    def warpper(*args, **kwargs):
        print("before my_func!!!")
        my_func(*args, **kwargs)
        print("after my_func!!!")
    return warpper
 
 
from functools import wraps


def decorator2(my_func):
    @wraps(my_func)
    def warpper(*args, **kwargs):
        print("before my_func!!!")
        my_func(*args, **kwargs)
        print("after my_func!!!")
    return warpper
 
@decorator1
def my_func1(a, b):
    print(a + b)
 
@decorator2
def my_func2(a, b):
    print(a + b)
 
print(my_func1.__name__)
# out: warpper
print(my_func2.__name__)
# out: my_func2
