http://web.eecs.utk.edu/~leparker/Courses/CS302-fall05/Notes/graph-searching.html

## Depth-first search

A search of a graph in which fringe vertices are visited in LIFO order (last-in, first-out):

- Depth-first search requires O(V + E) time if implemented with adjacency lists

- Pseudo-Code

```c++
const int UNSEEN = 0;
const int VISITED = 1;

void dfs(Vertex *v) {
  v->setVisited(VISITED);
  for (v->firstEdge(); !v->endOfEdges(); v->nextEdge()) {
    Vertex *w = v->getEdge()->getVertex2();
    if (w.getVisited() == UNSEEN)
      dfs(w)
  }
}
```

- Time Complexity

  1. Depth-first search requires O(V + E) time if implemented with adjacency lists

  2. Depth-first search requires O(V2) time if implemented with an adjacency matrix

  3. For a fully connected graph:

     <figure>
     <img src="https://i.imgur.com/rFy8W6O.png" alt="" style="width:40%;display:block;margin-left:auto;margin-right:auto;"/>
     <figcaption style="text-align:center"></figcaption>
     </figure>

     - V: Number of vertices
     - E: Number of edges
     - E = V \* (V-1) / 2

  4. For DFS over a matrix like this,

     ![Imgur](https://i.imgur.com/DWiI1YJ.png)

     - V = n \* m
     - E = n \* m \* 4 / 2 = n \* m \* 2
     - Time complexity: O(V+E) = O(2 \* n \* m + n \* m) = O(n \* m)

## Breadth-first search

A search of a graph in which fringe vertices are visited in FIFO order (first-in, first-out):

- Strategy: Remove vertices from the front of a queue and add their adjacent vertices to the back of
  the queue. This strategy ensures that while level l vertices are being processed, level l+1
  vertices are being added to the back of the queue. The level l+1 vertices will not be visited
  until all level l vertices have been exhausted.

- Pseudo-Code

```c++
const int UNSEEN = 0;
const int FRINGE = 1;
const int VISITED = 2;

bfs(Vertex *v) {
  dList<Vertex * v> unvisited;
  Vertex *current_vtx;

  while (!unvisited.empty()) {
    unvisited.first();
    current_vtx = unvisited.get();
    unvisited.deleteNode();
    current_vtx->setVisited(VISITED);

    for (current_vtx->firstEdge(); !current_vtx->endOfEdges();
         current_vtx->nextEdge()) {
      Vertex *w = current_vtx->getEdge()->getVertex2();
      if (w.getVisited() == UNSEEN) {
        w.setVisited(FRINGE) unvisited.append(w);
      }
    }
  }
}
```

- Time Complexity

  1. Breadth-first search requires O(V + E) time if implemented with adjacency lists

  2. Breadth-first search requires O(V2) time if implemented with an adjacency matrix

### Priority-first search (Dijkstra)

A search of a graph in which fringe vertices are assigned a priority and then visited in order of
highest priority (e.g., priority might be the number of miles from a starting vertex).

- Strategy:

  1.  When vertices are added to the fringe, add them to a priority queue.

  2.  Use deletemin/deletemax to remove vertices from the queue.

  3.  If a vertex's priority gets updated before it is removed from the fringe then move it to a new
      position in the queue

- Pseudo-Code

  1.  Before starting the search, assign a sentinel value as the priority of each vertex

  2.  Let VtxQueue be a priority queue

```c++
pfs(Vertex *v) {
  VtxQueue.insert(v);
  while (!VtxQueue.empty()) {
    current_vtx = VtxQueue.deletemin() {
      for (current_vtx->firstEdge(); !current_vtx->endOfEdges();
           current_vtx->nextEdge()) {
        Vertex *w = current_vtx->getEdge()->getVertex2();
        if (w->getVisited() != VISITED)
          if (VtxQueue.update(w, priority(current_vtx, w)))
            w->setValue(priority(current_vtx, w));
      }
    }
  }
```

- Time Complexity

  1.  Every vertex must be inserted into and deleted from the priority queue. Each insertion and
      deletion takes O(log V) time, where V is the number of vertices. Therefore the total number of
      insertions and deletions is O(V log V).

  2.  Every edge may update a vertice's priority and this update may require O(log V) time. Hence
      the total number of updates may require O(E log V) time.

  3.  The total running time is therfore O((E + V)log V). We have to include both V and E because we
      do not know which one will be greater. In general every vertex will have an edge, so normally
      E > V. However, in very sparse graphs where some vertices have no edges, E < V. Hence we have
      to include both the E and V terms in the final Big-O total.
