!!! [JavaScript RegExp Reference](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)

# Using String search() With a Regular Expression

Use a regular expression to do a case-insensitive search for "w3schools" in a string:

```js
var str = "Visit W3Schools";
var n = str.search(/w3schools/i);
```

The result in n will be: 6

# Using String search() With String

The search method will also accept a string as search argument. The string argument will be converted to a regular
expression:

```js
var str = "Visit W3Schools!";
var n = str.search("W3Schools");
```

# Use String replace() With a Regular Expression

Use a case insensitive regular expression to replace Microsoft with W3Schools in a string:

```js
var str = "Visit Microsoft!";
var res = str.replace(/microsoft/i, "W3Schools");
```

The result in res will be: Visit W3Schools!
