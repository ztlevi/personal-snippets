# Time complexity: O(|E|log|V|)
from typing import List


class Solution:
    def find_minimum_spanning_tree(self, n: int, edges: List[List[int]]) -> int:
        p = list(range(n))

        def find(x):
            if x != p[x]:
                p[x] = find(p[x])
            return p[x]

        cost = 0
        for u, v, w in sorted(edges, key=lambda x: x[2]):
            pu, pv = find(u), find(v)
            if pu == pv:
                continue
            p[pu] = pv
            cost += w
        return cost


so = Solution()
assert (
    so.find_minimum_spanning_tree(
        4, [[0, 1, 1], [0, 2, 6], [0, 3, 3], [1, 2, 4], [1, 3, 5], [2, 3, 2]]
    )
    == 6
)

assert (
    so.find_minimum_spanning_tree(
        5, [[0, 1, 1], [1, 2, 1], [2, 3, 2], [0, 3, 2], [0, 4, 3], [3, 4, 3], [1, 4, 6]]
    )
    == 7
)

print("Success")
