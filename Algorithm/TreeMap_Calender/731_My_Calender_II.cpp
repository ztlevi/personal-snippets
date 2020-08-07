class MyCalendarTwo {
public:
  MyCalendarTwo() {}

  bool book(int start, int end) {
    auto it = two.upper_bound(start);
    if (it != two.end() && end > it->second)
      return false;
    it = one.upper_bound(start);
    while (it != one.end()) {
      int s = it->second;
      int e = it->first;
      if (end < s)
        break;
      if (min(e, end) > max(s, start)) {
        two[min(e, end)] = max(s, start);
      }
      start = min(s, start);
      end = max(e, end); //合并成一个大的 one
      it = one.erase(it);
    }
    one[end] = start;
    return true;
  }

private:
  map<int, int> one, two;
};

// Not best solution
class MyCalendarTwo {
  vector<pair<int, int>> booked;
  vector<pair<int, int>> overlap;

public:
  MyCalendarTwo() {}

  bool book(int start, int end) {
    for (const auto &[os, oe] : overlap) {
      if (max(os, start) < min(oe, end)) {
        // cout << os << " " << oe << endl;
        return false;
      }
    }
    for (const auto &[s, e] : booked) {
      int ss = max(s, start);
      int ee = min(e, end);
      if (ss < ee)
        overlap.emplace_back(make_pair(ss, ee));
    }
    booked.emplace_back(make_pair(start, end));
    return true;
  }
};

/**
 * Your MyCalendarTwo object will be instantiated and called as such:
 * MyCalendarTwo* obj = new MyCalendarTwo();
 * bool param_1 = obj->book(start,end);
 */
