# https: // leetcode.com/problems/course-schedule-ii/description/
# Time Complexity: O(V+E).
# The above algorithm is simply DFS with an extra stack. So time complexity is the same as DFS which is.


class Solution:
    def findOrder(self, numCourses, prerequisites):
        """
        :type numCourses: int
        :type prerequisites: List[List[int]]
        :rtype: List[int]
        """
        in_coming_edges = collections.defaultdict(
            int
        )  # current course : its prerequsites count
        sucs = collections.defaultdict(set)  # prerequsite's successor

        for pre in prerequisites:
            cur = pre[0]
            pre_course = pre[1]
            sucs[pre_course].add(cur)
            in_coming_edges[cur] += 1

        no_incoming_edges = collections.deque(
            [course for course in range(numCourses) if in_coming_edges[course] == 0]
        )
        order = []
        while no_incoming_edges:
            c = no_incoming_edges.popleft()
            order.append(c)
            for suc in sucs[c]:
                in_coming_edges[suc] -= 1
                if in_coming_edges[suc] == 0:
                    no_incoming_edges.append(suc)
        if len(order) == numCourses:
            return order
        else:
            return []
