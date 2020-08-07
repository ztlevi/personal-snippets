// https://leetcode.com/problems/my-calendar-i/discuss/109475/JavaC%2B%2B-Clean-Code-with-Explanation

class MyCalendar {
  map<int, int> _books;

public:
  MyCalendar() {}
  bool book(int start, int end) {
    auto it = _books.lower_bound(start);
    if (it != _books.begin())
      --it;
    for (; it != _books.lower_bound(end); ++it) {
      if (it->second > start)
        return false;
    }
    // if (it!=_books.end() && it->second < end) return false;

    _books[start] = end;
    return true;
  }
};

// (Recommend) Using End
class MyCalendar {
  map<int, int> events;
  MyCalendar() {}
  bool book(int start, int end) {
    auto it = events.upper_bound(start);
    if (it != events.end() && it->second < end)
      return false;
    events.insert({end, start});
    return true;
  }
};
