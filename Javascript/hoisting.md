### Explain "hoisting".

Hoisting is a term used to explain the behavior of variable declarations in your code. Variables
declared or initialized with the `var` keyword will have their declaration "hoisted" up to the top
of the current scope. However, only the declaration is hoisted, the assignment (if there is one),
will stay where it is. Let's explain with a few examples.

```js
// var declarations are hoisted.
console.log(foo); // undefined
var foo = 1;
console.log(foo); // 1

// let/const declarations are NOT hoisted.
console.log(bar); // ReferenceError: bar is not defined
let bar = 2;
console.log(bar); // 2
```

Function declarations have the body hoisted while the function expressions (written in the form of
variable declarations) only has the variable declaration hoisted.

```js
// Function Declaration
console.log(foo); // [Function: foo]
foo(); // 'FOOOOO'
function foo() {
  console.log("FOOOOO");
}
console.log(foo); // [Function: foo]

// Function Expression
console.log(bar); // undefined
bar(); // Uncaught TypeError: bar is not a function
var bar = function () {
  console.log("BARRRR");
};
console.log(bar); // [Function: bar]
```
