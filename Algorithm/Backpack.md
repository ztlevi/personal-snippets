# Backpack

Given n items with size Ai, an integer m denotes the size of a backpack. How full you can fill this backpack? Example

If we have 4 items with size [2, 3, 5, 7], the backpack size is 11, we can select [2, 3, 5], so that the max size we can
fill this backpack is 10. If the backpack size is 12. we can select [2, 3, 7] so that we can fulfill the backpack.

You function should return the max size we can fill in the given backpack. Note

You can not divide any item into small pieces.

```python
class Solution(object):
    def back_pack(self, m, A):
        result = [[0 for _ in range(m+1)] for _ in range(len(A))]
        for i in range(len(A)):
            for j in range( m+1):
                if i == 0:
                    if  j >= A[i]:
                        result[i][j] = A[i]
                    continue
                elif j < A[i]:
                    result[i][j] = result[i-1][j]
                else:
                    result[i][j] = max(result[i-1][j - A[i]] + A[i], result[i-1][j])
        return result[len(A)-1][m]
```

```python
class Solution(object):
    def back_pack(self, m, A):
        result = [0] * (m + 1)
        for i in range(len(A)):
            for j in range(m, A[i]-1):
                result[j] = max(result[j - A[i]] + A[i], result[j])
        return result[m]
```

# Backpack II

Given n items with size Ai and value Vi, and a backpack with size m. What's the maximum value can you put into the
backpack? Example

Given 4 items with size [2, 3, 5, 7] and value [1, 5, 2, 4], and a backpack with size 10. The maximum value is 9. Note

You cannot divide item into small pieces and the total size of items you choose should smaller or equal to m.

```python
class Solution(object):
    def back_pack(self, m, A, V):
        result = [[0 for _ in range(m+1)] for _ in range(len(A))]
        for i in range(len(A)):
            for j in range(m+1):
                if i == 0:
                    if  j >= A[i]:
                        result[i][j] = V[i]
                    continue
                elif j < A[i]:
                    result[i][j] = result[i-1][j]
                else:
                    result[i][j] = max(result[i-1][j - A[i]] + V[i], result[i-1][j])
        return result[len(A)-1][m]
```

```python
class Solution(object):
    def back_pack(self, m, A, V):
        result = [0] * (m + 1)
        for i in range(len(A)):
            for j in range(m, A[i]-1):
                result[j] = max(result[j - A[i]] + V[i], result[j])
        return result[m]
```
