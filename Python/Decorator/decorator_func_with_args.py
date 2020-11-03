from functools import wraps


def dec(*a, **kw):
    def decorator(my_func):
        @wraps(my_func)
        def warpper(*args, **kwargs):
            print(*a)
            print(**kw)
            for i in range(kw[n]):
                print("wrapper!!")
                #print(**kwargs)
                my_func(*args, **kwargs)
        return warpper
    return decorator
 
@dec(1,2,3, n=4, m=5)
def my_func(a, b):
    print(a + b)
# my_func = dec(1,2,3,n=4,m=5)(my_func)(a, b)
 
my_func(1,2)
 
# output:
# (1, 2, 3)
# {'m': 5, 'n': 4}
# wrapper!!
# 3
# wrapper!!
# 3
# wrapper!!
# 3
# wrapper!!
# 3
