## http://2ality.com/2011/10/strict-mode-hatred.html

# Work-arounds for missing features

Strict mode eliminates some of JavaScript’s more questionable features. Here is a list of what is missing and how to
work around it:

- No more with statement. This statement causes performance and security problems [2]. Work-around: Instead of

```js
    with (obj.foo) {
        bar = ...;
    }
```

you can use an IIFE [3] and write

```js
    (function (o) {
        o.bar = ...;
    }(obj.foo));
```

- No more arguments.caller. This property was removed due to security concerns (unsafe code should not be able to access
  its caller). There is no replacement for it, you’ll have to introduce an additional parameter.

- No more arguments.callee. This property offers a convenient way of accessing the current function inside the function
  itself, without referring to a global variable or a variable in the surrounding scope.

```
    var MyMath = {
        fac: function (n) {
            if (n <= 0) {
                return 1;
            } else {
                return n * arguments.callee(n-1);
            }
        }
    };
```

A named function expression looks like a function declaration (a statement), but is in fact an expression. It gives the
function a name that is only accessible from within the function. That allows you to rewrite the above code as follows:

```js
var MyMath = {
  fac: function me(n) {
    if (n <= 0) {
      return 1;
    } else {
      return n * me(n - 1);
    }
  },
};
```

- No more global access via this. This has always been more of a bug than a feature – you could access the global object
  via this in non-method functions. This can lead to accidentally creating global variables (e.g. if you call a
  constructor without new). The following is an example of legitimate use:

```js
    (function () {
        // private data
        ...
        // public data
        this.myModule = { // avoid!
            ...
        };
    }());
```

Strict mode does not allow the above – this is undefined in non-method functions. You can use the following work-around:

```js
    (function (global) {
        // private data
        ...
        // public data
        global.myModule = {
            ...
        };
    }(this)); // top-level |this| points to global object
```

But you might not even need to access the global object inside the IIFE:

```js
    this.myModule = function () {
        // private data
        ...
        // public data
        return {
            ...
        };
    }();
```

- No more octal numbers. Now 0100 really is 100 and not 64. And 08 is not an error, any more. I can’t imagine that
  anyone misses octals.

All other changes effected by strict mode, such as preventing duplicate parameter names, only serve to check more
stringently for errors.

# Related reading

1. JavaScript’s strict mode: a summary
2. JavaScript’s with statement and why it’s deprecated
3. JavaScript variable scoping and its pitfalls
