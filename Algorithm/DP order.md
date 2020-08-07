## https://leetcode.com/problems/longest-palindromic-subsequence/

- s[i] = s[j]: dp[i][j] = dp[i+1][j-1] + 2
- else dp[i][j] = max(dp[i+1][j], dp[i][j-1])

```python
# recommend
for i in range(l-1, -1, -1):
    dp[i][i] = 1
    for j in range(i+1, l):

# Or
for j in range(l):
    dp[j][j] = 1
    for i in range(j-1, -1, -1):
```

if items number are limited, then it should reverse the order direction. See backpack problem.
