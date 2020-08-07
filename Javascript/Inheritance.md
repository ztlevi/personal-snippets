```js
function Employee(name) { this.name = name; }

Employee.prototype.getName = function() { return this.name; }

var emp1 = new Employee("Jim")

emp1.getName()
>> "Jim"

function Manager(name, dept) { this.name = name; this.dept = dept; }

Manage.prototype.getDept = function() { return this.dept; }

var mgr = new Manager("Machael", "Sales");

mgr.getDept();
>> "Sales"

mgr.getName();
>> VM11803:1 Uncaught TypeError: mgr.getDept is not a function

// method1
mgr.__proto__.__proto__ = Employee.prototype
// method2
Manager.prototype.__proto__ = Employee.prototype

mgr.getName();
>> "Machael"

var mgr2 = new Manager("Liz", "Marketing");
mgr2.getName();
>> "Liz"
```

![](https://ws2.sinaimg.cn/large/006tNc79gy1founobsr55j310m0oggoa.jpg)
