### Explain `Function.prototype.bind`.

Taken word-for-word from
[MDN](https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Global_objects/Function/bind):

fun.bind(thisArg[, arg1[, arg2[, ...]]])

```js
var module = {
  x: 42,
  getX: function () {
    return this.x;
  },
};

var retrieveX = module.getX;
console.log(retrieveX()); // The function gets invoked at the global scope
// expected output: undefined

var boundGetX = retrieveX.bind(module);
console.log(boundGetX());
// expected output: 42
```

### What's the difference between `.call` and `.apply`?

Both `.call` and `.apply` are used to invoke functions and the first parameter will be used as the
value of `this` within the function. However, `.call` takes in a comma-separated arguments as the
next arguments while `.apply` takes in an array of arguments as the next argument. An easy way to
remember this is C for `call` and comma-separated and A for `apply` and array of arguments.

```js
function add(a, b) {
  return a + b;
}

console.log(add.call(null, 1, 2)); // 3
console.log(add.apply(null, [1, 2])); // 3
```
