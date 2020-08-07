## https://leetcode.com/problems/kth-largest-element-in-an-array/description/

```python
import random
class Solution(object):

    # O(n) time, quick selection
    def findKthLargest(self, nums, k):
        # convert the kth largest to smallest
        return self.findKthSmallest(nums, len(nums)-k)

    def findKthSmallest(self, nums, k):
        if not nums:
            return
        pos = self.partition(nums, 0, len(nums)-1)
        pos_start = pos - 1
        while pos_start > 0 and nums[pos_start] == nums[pos]:
            pos_start -= 1
        d = pos - pos_start # duplicate length

        if k > pos:
            return self.findKthSmallest(nums[pos:], k-pos)
        elif k < pos_start + 1:
            return self.findKthSmallest(nums[:pos_start+1], k)
        else:
            return nums[pos]

    def partition(self, nums, l, r):
        mid = l
        p = random.randrange(l, r+1)
        pv = nums[p]
        nums[r], nums[p] = nums[p], nums[r]
        for i in range(l, r):
            if nums[i] <= pv:
                nums[i], nums[mid] = nums[mid], nums[i]
                mid += 1
        nums[r], nums[mid] = nums[mid], nums[r]
        return mid
```

```python
class Solution:
# @param {integer[]} nums
# @param {integer} k
# @return {integer}
    def findKthLargest(self, nums, k):
        # QuickSelect idea: AC in 52 ms
        # ---------------------------
        #
        pivot = nums[0]
        left  = [l for l in nums if l < pivot]
        equal = [e for e in nums if e == pivot]
        right = [r for r in nums if r > pivot]

        if k <= len(right):
            return self.findKthLargest(right, k)
        elif (k - len(right)) <= len(equal):
            return equal[0]
        else:
            return self.findKthLargest(left, k - len(right) - len(equal))
```
