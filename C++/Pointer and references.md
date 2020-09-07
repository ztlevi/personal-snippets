# Pointer and references

### Pointer

- A pointer can be initialized to any value anytime after it is declared.

  ```c++
  int a = 5;// some codeint 
  *p = a;
  ```

- A pointer can be assigned to point to a _NULL_ value.

- Pointers need to be dereferenced with a `*`.

- A pointer can be changed to point to any variable of the same type.

  Example:

  ```c++
  int a = 5;
  int *p;
  p = &a;
  int b = 6;
  p = &b;
  ```

### Reference

- A reference must be initialized when it is declared.

  ```c++
  int a = 5;
  int &ref = a;
  ```

- References cannot be _NULL_.

- References can be used ,simply, by name.

- Once a reference is initialized to a variable, it cannot be changed to refer to a variable object.
