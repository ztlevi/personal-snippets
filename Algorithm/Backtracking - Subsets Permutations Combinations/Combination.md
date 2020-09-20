# Combination

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
