###############################################################################
#  Useful functions from itertools: islice, takewhile, dropwhile              #
###############################################################################

# takewhile dropwhile
from itertools import dropwhile, islice, takewhile

print(list(takewhile(lambda x: x < 10, fib())))

# >>> [1, 1, 2, 3, 5, 8]

print(list(dropwhile(lambda x: x < 5,
                     takewhile(lambda x: x < 100,
                               islice(fib(), 15)))))

# >>> [5, 8, 13, 21, 34, 55, 89]

# islice


def fib(a=1, b=1):
    while True:
        yield a
        a, b = b, a + b


print(list(islice(fib(), 10)))

# >>> [1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
