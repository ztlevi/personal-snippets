Take a quick second to guess what the output of the following snippet is:

```javascript
for (var i = 0; i < 10; i++) {
  setTimeout(function () {
    console.log(i);
  }, 100 * i);
}
```

Output:

```javascript
10;
10;
10;
10;
10;
10;
10;
10;
10;
10;
```

A common work around is to use an IIFE - an Immediately Invoked Function Expression - to capture i
at each iteration:

```javascript
for (var i = 0; i < 10; i++) {
  // capture the current state of 'i'
  // by invoking a function with its current value
  (function (i) {
    setTimeout(function () {
      console.log(i);
    }, 100 * i);
  })(i);
}
```

Output:

```javascript
0;
1;
2;
3;
4;
5;
6;
7;
8;
9;
```
