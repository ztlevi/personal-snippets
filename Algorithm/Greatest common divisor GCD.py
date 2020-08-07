# from fractions import gcd


def gcd(x, y):
    while y != 0:
        (x, y) = (y, x % y)
    return x


gcd(20, 8)
