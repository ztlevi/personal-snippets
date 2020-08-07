The complexity of bucket sort isn't constant depending on the input. However in the average case the complexity of the algorithm is O(n + k) where n is the length of the input sequence, while k is the number of buckets. The problem is that its worst-case performance is O(n^2) which makes it as slow as bubble sort
---

def bsort(A):
  """Returns A sorted. with A = {x : x such that 0 <= x < 1}."""
    buckets = [[] for x in range(10)]
    for i, x in enumerate(A):
        buckets[int(x*len(buckets))].append(x)
    out = []
    for buck in buckets:
        out += isort(buck)
    return out
    
def isort(A):
    if len(A) <= 1: return A
    i = 1
    while i < len(A):
        k = A[i]
        j = i - 1
        while j >= 0 and A[j] > k:
            A[j+1] = A[j]
            A[j] = k
            j -= 1      
        i += 1
    return A