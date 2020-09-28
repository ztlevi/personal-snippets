# Mutable

## Remarks[](https://docs.microsoft.com/en-us/cpp/cpp/mutable-data-members-cpp?view=vs-2019#remarks)

For example, the following code will compile without error because `m_accessCount` has been declared
to be **mutable**, and therefore can be modified by `GetFlag` even though `GetFlag` is a const
member function.

```cpp
// mutable.cpp
class X {
public:
  bool GetFlag() const {
    m_accessCount++;
    return m_flag;
  }

private:
  bool m_flag;
  mutable int m_accessCount;
};

int main() {}
```
