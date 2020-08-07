#include <bits/stdc++.h>
using namespace std;
class MyCalendarThree {
public:
  MyCalendarThree() {}

  map<int, int> count = {{-1, 0}};
  int res = 0;
  int book(int s, int e) {
    //    auto start = count.emplace(s, (--count.lower_bound(s))->second).first;
    //    auto end = count.emplace(e, (--count.lower_bound(e))->second).first;
    //    for (auto i = start; i != end; ++i) res = max(res, ++(i->second));
    if (count.find(s) == count.end())
      count[s] = (--count.lower_bound(s))->second;
    if (count.find(e) == count.end())
      count[e] = (--count.lower_bound(e))->second;
    for (auto i = count.find(s); i != count.find(e); ++i) {
      res = max(res, ++(i->second));
    }
    cout << res << endl;
    return res;
  }
};
