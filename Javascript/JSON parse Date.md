Parsing Dates

you can use the second parameter, of the JSON.parse() function, called reviver.

The reviver parameter is a function that checks each property, before returning the value.

```js
var text = '{ "name":"John", "birth":"1986-12-14", "city":"New York"}';
var obj = JSON.parse(text, function(key, value) {
  if (key == "birth") {
    return new Date(value);
  } else {
    return value;
  }
});

document.getElementById("demo").innerHTML = obj.name + ", " + obj.birth;
```
