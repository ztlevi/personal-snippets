# Weighted quick-union with path compression.

Find needs to trace references to the root in time O(h), where h is the height of the tree

Union takes O(1) time.

```c++
class UnionFind {
  map<int, int> p;
  map<int, int> rank;
  UnionFind() {}

  void merge(int a, int b) {
    int pa = find(a);
    int pb = find(b);
    if (pa == pb) return;
    if (rank.find(pa) == rank.end()) rank[pa] = 1;
    if (rank.find(pb) == rank.end()) rank[pb] = 1;
    if (rank[pa] < rank[pb]) {
      m[pa] = pb;
      rank[pb] += rank[pa];
    } else {
      m[pb] = pa;
      rank[pa] += rank[pb];
    }
  }
  int find(int a) {
    if (p.find(a) == p.end()) p[a] = a;
    if (p[a] != a) {
      p[a] = find(p[a]);
    }
    return p[a];
  }

```

```python
class UnionFind:
    def __init__(self):
        self.parent = collections.defaultdict(int)
        self.rank = collections.defaultdict(lambda : 1)
    def find(self, x):
        if x not in self.parent:
            self.parent[x] = x
        if self.parent[x] != x:
            self.parent[x] = self.find(self.parent[x])
        return self.parent[x]

    def union(self, a, b):
        pa = self.find(a)
        pb = self.find(b)
        if pa == pb:
            return
        elif self.rank[pa] > self.rank[pb]:
            self.rank[pa] += self.rank[pb]
            self.parent[pb] = pa
        else:
            self.rank[pb] += self.rank[pa]
            self.parent[pa] = pb
```

```python
class UnionFind:
    """Weighted quick-union with path compression.
    The original Java implementation is introduced at
    https://www.cs.princeton.edu/~rs/AlgsDS07/01UnionFind.pdf
    >>> uf = UnionFind(10)
    >>> for (p, q) in [(3, 4), (4, 9), (8, 0), (2, 3), (5, 6), (5, 9),
    ...                (7, 3), (4, 8), (6, 1)]:
    ...     uf.union(p, q)
    >>> uf._id
    [8, 3, 3, 3, 3, 3, 3, 3, 3, 3]
    >>> uf.find(0, 1)
    True
    >>> uf._id
    [3, 3, 3, 3, 3, 3, 3, 3, 3, 3]
    """

    def __init__(self, n):
        self.ds = [-1] * n

    def find(self, a):
        if self.ds[a] < 0:
            return a
        self.ds[a] = self.find(self.ds[a])
        return self.ds[a]

    def union(self, a, b):
        pa = self.find(a)
        pb = self.find(b)
        if pa == pb:
            return
        if self.ds[pa] < self.ds[pb]:
            self.ds[pa] += self.ds[pb]
            self.ds[pb] = pa
        else:
            self.ds[pb] += self.ds[pa]
            self.ds[pa] = pb
```
