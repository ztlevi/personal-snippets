# Static

```js
class Point {
  constructor(x, y) {
    this.x = x;
    this.y = y;
  }

  static distance(a, b) {
    const dx = a.x - b.x;
    const dy = a.y - b.y;

    return Math.hypot(dx, dy);
  }
}

const p1 = new Point(5, 5);
const p2 = new Point(10, 10);

console.log(Point.distance(p1, p2)); // 7.0710678118654755
```

# Boxing with prototype and static methods

When a static or prototype method is called without an object valued "this", then the "this" value
will be undefined inside the called function. Autoboxing will not happen. The behavior will be the
same even if we write the code in non-strict mode because all the functions, methods, constructor,
getters or setters are executed in strict mode. So if we do not specify this value then the this
value will be undefined.

```js
class Animal {
  speak() {
    return this;
  }
  static eat() {
    return this;
  }
}

let obj = new Animal();
obj.speak(); // Animal {}
let speak = obj.speak;
speak(); // undefined

Animal.eat(); // class Animal
let eat = Animal.eat;
eat(); // undefined
```

If we write the above code using traditional function based classes, then autoboxing will happen
based on the "this" value for which the function was called.

```js
function Animal() {}

Animal.prototype.speak = function () {
  return this;
};

Animal.eat = function () {
  return this;
};

let obj = new Animal();
let speak = obj.speak;
speak(); // global object

let eat = Animal.eat;
eat(); // global object
```
