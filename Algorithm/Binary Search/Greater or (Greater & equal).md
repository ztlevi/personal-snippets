https://leetcode.com/problems/longest-increasing-subsequence/description
http://www.lintcode.com/en/problem/search-for-a-range/

# Python bisect

```python
bisect.bisect_left(A, x, lo, hi) # all e in a[lo:hi] have e >= x
bisect.bisect_right(A, x, lo, hi) # all e in a[lo:hi] have e > x
```

Consider, when we have lo = 1 and hi = 2.

- If I used, int mi = (lo + hi) // 2; mi = 1
- If I used, int mi = (lo + hi + 1) // 2; mi = 2

Now since, lo = mi, we will keep falling back to lo = 1 and hi = 2(Hence TLE), if I used first
version.

General rule of thumb I use:

- If we are doing lo = mi, and hi = mi-1, use 2nd version.
- If we are doing hi = mi, lo = mi + 1, use 1st version.

## bisect_right

```python
def bisect_right(a, x, lo=0, hi=None):
    """Return the index where to insert item x in list a, assuming a is sorted.

    The return value i is such that all e in a[:i] have e <= x, and all e in
    a[i:] have e > x.  So if x already appears in the list, a.insert(x) will
    insert just after the rightmost x already there.

    Optional args lo (default 0) and hi (default len(a)) bound the
    slice of a to be searched.
    """

    if lo < 0:
        raise ValueError('lo must be non-negative')
    if hi is None:
        hi = len(a)
    while lo < hi:
        mid = (lo+hi)//2
        if x < a[mid]: hi = mid
        else: lo = mid+1
    return lo
```

## bisect_left

```python
def bisect_left(a, x, lo=0, hi=None):
    """Return the index where to insert item x in list a, assuming a is sorted.

    The return value i is such that all e in a[:i] have e < x, and all e in
    a[i:] have e >= x.  So if x already appears in the list, a.insert(x) will
    insert just before the leftmost x already there.

    Optional args lo (default 0) and hi (default len(a)) bound the
    slice of a to be searched.
    """

    if lo < 0:
        raise ValueError('lo must be non-negative')
    if hi is None:
        hi = len(a)
    while lo < hi:
        mid = (lo+hi)//2
        if a[mid] < x: lo = mid+1
        else: hi = mid
    return lo
```
