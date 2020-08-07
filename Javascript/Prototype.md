![](https://ws4.sinaimg.cn/large/006tNc79gy1foujdng6otj31km0pamzv.jpg)

prototype will not be duplicated when creating new objects, but function's variables or functions will.

```js
function foo() {}

foo.prototype.s = "This is test string."

var a = new foo();

foo.prototype
>> {s: "This is test string.", constructor: ƒ}

foo.prototype === a.__proto__
>> true


// prototype has a constructor reference
var proto = foo.prototype

proto.constructor
>> function foo()

// Two ways to create Object
var o1 = {};
var o2 = new Object();
o2.__proto__ === Object.prototype
>> true

o1.__proto__ === o2.__proto__
>> true

```

![](https://ws3.sinaimg.cn/large/006tNc79gy1foumx1r8qrj312c0jutb2.jpg)

```js
function Employee() {}
var emp = new Employee();

emp.__proto__.__proto__ === Object.prototype;
```

![](https://ws1.sinaimg.cn/large/006tNc79gy1foun48rjnrj30y80j6abl.jpg)
