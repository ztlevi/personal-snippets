#include <cstdio>
#include <vector>

struct S {
  S(int) { puts("S(int)"); }
  S() { puts("S()"); }
  S(const S &) { puts("S(const S &)"); }
  // Move constructor
  S(S &&) { puts("S&&"); } // && is rvalue reference declarator

  S &operator=(const S &) {
    puts("operator(const S &)");
    return *this;
  }

  S &operator=(S &&) {
    puts("opeartor=(S &&)");
    return *this;
  }

  ~S() { puts("~S()"); }
};

int main() {
  std::vector<S> vec;
  vec.push_back(S(3));
  // S(int)
  // S&&
  // ~S()
  // ~S()
  vec.emplace_back(3);
  // S(int)
  // ~S()
}
