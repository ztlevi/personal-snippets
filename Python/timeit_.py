timeit.timeit(
    "text = 'abcdefg'; s = bytearray(text); s[1] = 'Z'; str(s)", number=1000000
)
# 1.0387420654296875
