# Combination

## Bitmask

```python
import itertools


def gray_code(n):
    return n ^ (n >> 1)


def count_bits(n):
    cnt = 0
    while n:
        cnt += n & 1
        n = n >> 1
    return cnt


def mask2arr(nums, mask):
    res = []
    for i, n in enumerate(nums):
        if mask & 1 << i:
            res.append(n)
    return res


def combination(nums, k):
    res = []
    nl = len(nums)
    for i in range(1 << nl):
        if count_bits(i) == k:
            res.append(tuple(mask2arr(nums, i)))
    return res


nums = [1, 2, 3, 4, 5, 6, 7]


assert sorted(combination(nums, 5)) == sorted(itertools.combinations(nums, 5))
```

## Backtracking

```python
import itertools


def combination(nums, r):
    res = []

    def dfs(start, left, cur):
        if left == 0:
            res.append(tuple(cur))
            return
        for i in range(start, len(nums)):
            cur.append(nums[i])
            dfs(i + 1, left - 1, cur)
            cur.pop()

    dfs(0, r, [])
    print(f"res: {res}")
    return res


nums = [1, 2, 3, 4, 5]

assert combination(nums, 3) == list(itertools.combinations(nums, 3))
```
