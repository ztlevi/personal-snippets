# Time complexity: O(|E|log|V|)
import collections
import heapq
from typing import List


class Solution:
    def find_minimum_spanning_tree(self, n: int, edges: List[List[int]]) -> int:
        graph = collections.defaultdict(lambda: collections.defaultdict(int))

        for u, v, w in edges:
            graph[u][v] = w
            graph[v][u] = w

        q = [(0, 0)]
        cost = 0
        seen = set()
        while q:
            w, u = heapq.heappop(q)
            if u in seen:
                continue
            cost += w
            seen.add(u)
            if len(seen) == n:
                break
            for v, w in graph[u].items():
                if v in seen:
                    continue
                heapq.heappush(q, (w, v))

        return cost if len(seen) == n else -1


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
