# The Power of @property

The pythonic way to deal with the above problem is to use property. Here is how we could have achieved it.

```python
class Celsius:
    def __init__(self, temperature = 0):
        self.temperature = temperature

    def to_fahrenheit(self):
        return (self.temperature * 1.8) + 32

    def get_temperature(self):
        print("Getting value")
        return self._temperature

    def set_temperature(self, value):
        if value < -273:
            raise ValueError("Temperature below -273 is not possible")
        print("Setting value")
        self._temperature = value

    temperature = property(get_temperature,set_temperature)
```

Also, you could write in this way:

```python
# make empty property
temperature = property()
# assign fget
temperature = temperature.getter(get_temperature)
# assign fset
temperature = temperature.setter(set_temperature)
```

## Dig deeper

In Python, property() is a built-in function that creates and returns a property object. The signature of this function
is

```python
property(fget=None, fset=None, fdel=None, doc=None)
```

where, fget is function to get value of the attribute, fset is function to set value of the attribute, fdel is function
to delete the attribute and doc is a string (like a comment). As seen from the implementation, these function arguments
are optional. So, a property object can simply be created as follows.

```python
class Employee(object):
    def __init__(self, first, last):
        self.first = first
        self.last = last

    @property
    def email(self):
        return '{}.{}@email.com'.format(self.first, self.last)

    @property
    def fullname(self):
        return '{} {}'.format(self.first, self.last)

    @fullname.setter
    def fullname(self, name):
        first, last = name.split(' ')
        self.first = first
        self.last = last

    @fullname.deleter
    def fullname(self):
        print('Delete name!')
        self.first = None
        self.last = None

emp_1 = Employee('John', 'Smith')

emp_1.fullname = 'Corey Schafer'

del emp_1.fullname

print(emp_1.first)
print(emp_1.email)
print(emp_1.fullname)
```

## Decorators

Programmers familiar with [decorators in Python](https://www.programiz.com/python-programming/decorator) can recognize
that the above construct can be implemented as decorators.

We can further go on and not define names `get_temperature` and `set_temperature` as they are unnecessary and pollute
the class namespace. For this, we reuse the name `temperature` while defining our getter and setter functions. This is
how it can be done.

```python
class Celsius:
    def __init__(self, temperature = 0):
        self._temperature = temperature

    def to_fahrenheit(self):
        return (self.temperature * 1.8) + 32

    @property
    def temperature(self):
        print("Getting value")
        return self._temperature

    @temperature.setter
    def temperature(self, value):
        if value < -273:
            raise ValueError("Temperature below -273 is not possible")
        print("Setting value")
        self._temperature = value
c = Celsius(4)
print(c.temperature)
```
