# Permutations II (contains duplicates) : https://leetcode.com/problems/permutations-ii/

```python
class Solution(object):
    def permuteUnique(self, nums):
        """
        :type nums: List[int]
        :rtype: List[List[int]]
        """

        def helper(nums, cur, used):
            if len(cur) == len(nums):
                res.append(list(cur))
            for i in range(len(nums)):
                if used[i] or (i > 0 and nums[i] == nums[i-1] and not used[i-1]):
                    continue
                cur.append(nums[i])
                used[i] = True
                helper(nums, cur, used)
                cur.pop()
                used[i] = False

        res = []
        nums.sort()
        used = [False] * len(nums)
        helper(nums, [], used)
        return res
```

## Use next_permutation

Next permutation:

1. Find the largest index k such that nums[k] < nums[k + 1]. If no such index exists, just reverse nums and done.
2. Find the largest index l > k such that nums[k] < nums[l].
3. Swap nums[k] and nums[l].
4. Reverse the sub-array nums[k + 1:].

```c++
class Solution {
public:
    vector<vector<int>> permuteUnique(vector<int>& nums) {
        sort(nums.begin(), nums.end());
        vector<vector<int>> res;
        do {
            res.emplace_back(nums);
        } while (nextPermutation(nums));
        return res;
    }
    bool nextPermutation(vector<int> &nums) {
      int n = nums.size(), k, l;
      for (k = n - 2; k >= 0; k--) {
        if (nums[k] < nums[k + 1]) {
          break;
        }
      }
      if (k < 0) {
        reverse(nums.begin(), nums.end());
        return false;
      } else {
        for (l = n - 1; l > k; l--) {
          if (nums[l] > nums[k]) {
            break;
          }
        }
        swap(nums[k], nums[l]);
        reverse(nums.begin() + k + 1, nums.end());
        return true;
      }
    }
};
```
