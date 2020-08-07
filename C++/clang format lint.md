# Clang format and linter

- Installation

```
pip3 install cpplint
```

- Dump config

```
clang-format -style=google -dump-config > .clang-format
```

- Format all

```
clang-format -i -style=file *.c *.cc *.cpp *.h *.hh *.hpp
```
