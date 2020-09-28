#!/usr/bin/env python3

# https://www.geeksforgeeks.org/strongly-connected-components/
# Python implementation of Kosaraju's algorithm to print all SCCs

from collections import defaultdict


# This class represents a directed graph using adjacency list representation
class Graph:
    def __init__(self, vertices):
        self.V = vertices  # No. of vertices
        self.graph = defaultdict(list)  # default dictionary to store graph

    # function to add an edge to graph
    def addEdge(self, u, v):
        self.graph[u].append(v)

    # A function used by DFS
    def DFSUtil(self, v, visited, path):
        # Mark the current node as visited and print it
        visited[v] = True
        path.append(v)
        # Recur for all the vertices adjacent to this vertex
        for i in self.graph[v]:
            if visited[i] == False:
                self.DFSUtil(i, visited, path)

    def fillOrder(self, v, visited, stack):
        # Mark the current node as visited
        visited[v] = True
        # Recur for all the vertices adjacent to this vertex
        for i in self.graph[v]:
            if visited[i] == False:
                self.fillOrder(i, visited, stack)
        stack = stack.append(v)

    # Function that returns reverse (or transpose) of this graph
    def getTranspose(self):
        g = Graph(self.V)

        # Recur for all the vertices adjacent to this vertex
        for i in self.graph:
            for j in self.graph[i]:
                g.addEdge(j, i)
        return g

    # The main function that finds and prints all strongly
    # connected components
    def printSCCs(self):
        stack = []
        # Mark all the vertices as not visited (For first DFS)
        visited = [False] * (self.V)
        # Fill vertices in stack according to their finishing
        # times
        for i in range(self.V):
            if visited[i] == False:
                self.fillOrder(i, visited, stack)

        # Create a reversed graph
        gr = self.getTranspose()

        # Mark all the vertices as not visited (For second DFS)
        visited = [False] * (self.V)

        # Now process all vertices in order defined by Stack
        while stack:
            i = stack.pop()
            if visited[i] == False:
                path = []
                gr.DFSUtil(i, visited, path)
                print(path)


# Create a graph given in the above diagram
g = Graph(5)
g.addEdge(1, 0)
g.addEdge(0, 2)
g.addEdge(2, 1)
g.addEdge(0, 3)
g.addEdge(3, 4)


print("Following are strongly connected components " + "in given graph")
g.printSCCs()
# This code is contributed by Neelam Yadav


################ Shorter version ###############

# User function Template for python3
import collections


# Graph (adj) is a default dict of type list
# V is the number of vertices in the graph
def countSCCs(adj, V):
    def dfs1(i, vis, stk):
        vis[i] = True
        for j in adj[i]:
            if not vis[j]:
                dfs1(j, vis, stk)
        stk.append(i)

    def dfs2(i, vis, path):
        vis[i] = True
        path.append(i)
        for j in tadj[i]:
            if not vis[j]:
                dfs2(j, vis, path)

    tadj = collections.defaultdict(list)
    for u, vv in adj.items():
        for v in vv:
            tadj[v].append(u)

    stk = []
    vis = [False] * V
    for i in range(V):
        if not vis[i]:
            dfs1(i, vis, stk)

    vis = [False] * V
    res = []
    for i in stk[::-1]:
        path = []
        if not vis[i]:
            dfs2(i, vis, path)
            res.append(path)
    return len(res)


adj = collections.defaultdict(list)
a = [0, 1, 1, 2, 2, 3]
for i in range(0, len(a), 2):
    adj[a[i]].append(a[i + 1])

res = countSCCs(adj, 4)
print(res)
