# Permutations

https://leetcode.com/problems/permutations/

```python
class Solution(object):
    def permute(self, nums):
        """
        :type nums: List[int]
        :rtype: List[List[int]]
        """

        def dfs(nums, i):
            if i == len(nums) - 1:
                res.append(list(nums))
                return

            for j in range(i, len(nums)):
                nums[i], nums[j] = nums[j], nums[i]
                dfs(nums, i+1)
                nums[j], nums[i] = nums[i], nums[j]
        res = []
        dfs(nums, 0)
        return res
```

```python
def permute(self, nums):
    perms = [[]]
    for n in nums:
        new_perms = []
        for perm in perms:
            for i in xrange(len(perm)+1):
                new_perms.append(perm[:i] + [n] + perm[i:])   ###insert n
        perms = new_perms
    return perms
```
