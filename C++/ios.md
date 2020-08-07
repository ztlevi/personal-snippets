# IOS Base

## cout.setf: https://devdocs.io/cpp/io/ios_base/setf

Flags, mask

```cpp
#include <iostream>
#include <iomanip>

const double PI = 3.1415926535;

int main()
{
    const int WIDTH = 15;

    std::cout.setf(std::ios::right);  //equivalent: cout << right;
    std::cout << std::setw(WIDTH/2) << "radius"
              << std::setw(WIDTH) << "circumference" << '\n';

    std::cout.setf(std::ios::fixed);
    for (double radius = 1; radius <= 6; radius += 0.5) {
        std::cout << std::setprecision(1) << std::setw(WIDTH/2)
                  << radius
                  << std::setprecision(2) << std::setw(WIDTH)
                  << (2 * PI * radius) << '\n';
    }
}
```

output:

```
radius  circumference
    1.0           6.28
    1.5           9.42
    2.0          12.57
    2.5          15.71
    3.0          18.85
    3.5          21.99
    4.0          25.13
    4.5          28.27
    5.0          31.42
    5.5          34.56
    6.0          37.70
```
