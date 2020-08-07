# Pointer and references

![](http://cdncontribute.geeksforgeeks.org/wp-content/uploads/pointer2pointer.png)

- Pointer

```cpp
  int var = 8;
  int *ptr = &var;
  *ptr = 3;
```

- References

```cpp
  int& ref = var;
  ref = 3;
```

- Double pointer

```cpp
  char* buffer = new char[8];
  char **pp = &buffer;
```
