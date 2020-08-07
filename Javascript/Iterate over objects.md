Under ECMAScript 5, you can combine Object.keys() and Array.prototype.forEach():

```
var obj = { first: "John", last: "Doe" };

Object.keys(obj).forEach(function(key) {
    console.log(key, obj[key]);
});
```

ES2016 adds for...of:

```
for (const key of Object.keys(obj)) {
    console.log(key, obj[key]);
}

// or
for (const value of obj) {
    console.log(value)
}
```

ES2017 adds Object.entries() which avoids having to look up each value in the original object:

```
Object.entries(obj).forEach(
    ([key, value]) => console.log(key, value)
);
Both Object.keys() and Object.entries() iterate properties in the same order as a for...in loop but ignore the prototype chain. Only the object's own enumerable properties are iterated.
```
