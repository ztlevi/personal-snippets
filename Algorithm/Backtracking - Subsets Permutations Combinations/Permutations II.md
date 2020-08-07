# Permutations II (contains duplicates) : https://leetcode.com/problems/permutations-ii/

```java
public List<List<Integer>> permuteUnique(int[] nums) {
    List<List<Integer>> list = new ArrayList<>();
    Arrays.sort(nums);
    backtrack(list, new ArrayList<>(), nums, new boolean[nums.length]);
    return list;
}

private void backtrack(List<List<Integer>> list, List<Integer> tempList, int [] nums, boolean [] used){
    if(tempList.size() == nums.length){
        list.add(new ArrayList<>(tempList));
    } else{
        for(int i = 0; i < nums.length; i++){
            if(used[i] || i > 0 && nums[i] == nums[i-1] && !used[i - 1]) continue;
            used[i] = true;
            tempList.add(nums[i]);
            backtrack(list, tempList, nums, used);
            used[i] = false;
            tempList.remove(tempList.size() - 1);
        }
    }
}
```

## Use next_permutation

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
