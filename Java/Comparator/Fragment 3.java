lambda
---

intervals.sort(Comaparator.comparing(i -> i.start));

Comparator.comparing((Person p)->p.firstName)
          .thenComparing(p->p.lastName)
          .thenComparingInt(p->p.age);
Comparator<Connection> cmp = Comparator.comparing((Connection c) -> c.cost)
                                       .thenComparing((c)->c.city1)
                                       .thenComparing((c)->c.city2);

//If you have accessor methods:
Comparator.comparing(Person::getFirstName)
          .thenComparing(Person::getLastName)
          .thenComparingInt(Person::getAge);

Comparator<Human> comparator
    = (h1, h2) -> h1.getName().compareTo(h2.getName());
    
humans.sort(comparator.reversed());

Comparator<Point> c = (Point p1, Point p2) ->
    {if (p1.x == p2.x) {
        return p1.y - p2.y;
    } else {
        return p1.x - p2.x;
    }};

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