# Numeric Sort

By default, the sort() function sorts values as strings.

This works well for strings ("Apple" comes before "Banana").

However, if numbers are sorted as strings, "25" is bigger than "100", because "2" is bigger than "1".

Because of this, the sort() method will produce incorrect result when sorting numbers.

You can fix this by providing a compare function:

```js
var points = [40, 100, 1, 5, 25, 10];
points.sort(function(a, b) {
  return a - b;
});
```

Use the same trick to sort an array descending:

```js
var points = [40, 100, 1, 5, 25, 10];
points.sort(function(a, b) {
  return b - a;
});
```

# Sorting an Array in Random Order

```js
var points = [40, 100, 1, 5, 25, 10];
points.sort(function(a, b) {
  return 0.5 - Math.random();
});
```

# Using Math.max() on an Array

You can use Math.max.apply to find the highest number in an array:

```js
function myArrayMax(arr) {
  return Math.max.apply(null, arr);
}
```

Math.max.apply([1, 2, 3]) is equivalent to Math.max(1, 2, 3).

# Sorting Object Arrays

JavaScript arrays often contain objects:

```js
var cars = [
  { type: "Volvo", year: 2016 },
  { type: "Saab", year: 2001 },
  { type: "BMW", year: 2010 },
];
```

Even if objects have properties of different data types, the sort() method can be used to sort the array.

The solution is to write a compare function to compare the property values:

```js
cars.sort(function(a, b) {
  return a.year - b.year;
});
```
