Sort from big to small sue b-a
---

// Definition for a point.
class Point {
    int x;
    int y;
    Point() { x = 0; y = 0; }
    Point(int a, int b) { x = a; y = b; }
}
 
PriorityQueue<Point> pq = new PriorityQueue<>(k, new Comparator<Point>() {
    @Override
    public int compare(Point p1, Point p2) {
        int diff = getDistance(p2, origin) - getDistance(p1, origin);
        if (diff == 0)
            diff = p2.x - p1.x;
        if (diff == 0)
            diff = p2.y - p1.y;
        return diff;
    }
});