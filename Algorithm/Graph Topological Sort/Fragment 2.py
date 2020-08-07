# https://leetcode.com/problems/course-schedule/description/
class Solution:
    def canFinish(self, numCourses: int, prerequisites: List[List[int]]) -> bool:
        l = numCourses
        cnt = [0] * l  # innodes count
        edges = [set() for _ in range(l)]  # graph
        for course, prerequisite in prerequisites:
            edges[prerequisite].add(course)
            cnt[course] += 1  # course : num of prerequisites

        queue = [i for i in range(l) if cnt[i] == 0]
        count = 0  # num of courses taken
        while queue:
            count += 1
            takeCourse = queue.pop(0)
            for course in edges[takeCourse]:
                cnt[course] -= 1
                if cnt[course] == 0:
                    queue.append(course)
        return count == l
