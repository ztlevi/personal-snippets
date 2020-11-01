# Nix expression language

## 15.1 Values

Antiquotation (`` ${_`expr`_} ``) is supported in indented strings.

Since `${` and `''` have special meaning in indented strings, you need a way to quote them. `$` can
be escaped by prefixing it with `''` (that is, two single quotes), i.e., `''$`. `''` can be escaped
by prefixing it with `'`, i.e., `'''`. `$` removes any special meaning from the following `$`.
Linefeed, carriage-return and tab characters can be written as `''\n`, `''\r`, `''\t`, and `''\`
escapes any other character. Summarizing the fragment

### rec set

```python
...
inherit x y z;
inherit (src-set) a b c;
...
```

is equivalent to

```python
...
x = x; y = y; z = z;
a = src-set.a; b = src-set.b; c = src-set.c;
...
```

when used while defining local variables in a let-expression or while defining a set.

## 15.3 Oeprators

https://nixos.org/manual/nix/stable/#sec-language-operators

## 15.5 Builtins

https://nixos.org/manual/nix/stable/#ssec-builtins
