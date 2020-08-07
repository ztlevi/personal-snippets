The constructor property returns the constructor function for all JavaScript variables.

```js
"John".constructor                // Returns function String()  {[native code]}
(3.14).constructor                // Returns function Number()  {[native code]}
false.constructor                 // Returns function Boolean() {[native code]}
[1,2,3,4].constructor             // Returns function Array()   {[native code]}
{name:'John',age:34}.constructor  // Returns function Object()  {[native code]}
new Date().constructor            // Returns function Date()    {[native code]}
function () {}.constructor        // Returns function Function(){[native code]}
```

You can check the constructor property to find out if an object is an Array (contains the word "Array"):

```js
function isArray(myArray) {
  return myArray.constructor.toString().indexOf("Array") > -1;
}
```

Or even simpler, you can check if the object is an Array function:

```js
function isArray(myArray) {
  return myArray.constructor === Array;
}
```
