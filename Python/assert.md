## The assert Statement:

When it encounters an assert statement, Python evaluates the accompanying expression, which is hopefully true. If the
expression is false, Python raises an AssertionError exception.

The syntax for assert is:

```python
assert Expression[, Arguments]
```

If the assertion fails, Python uses ArgumentExpression as the argument for the AssertionError. AssertionError exceptions
can be caught and handled like any other exception using the try-except statement, but if not handled, they will
terminate the program and produce a traceback.

## Example:

Here is a function that converts a temperature from degrees Kelvin to degrees Fahrenheit. Since zero degrees Kelvin is
as cold as it gets, the function bails out if it sees a negative temperature:

```python
#!/usr/bin/python3

def KelvinToFahrenheit(Temperature):
   assert (Temperature >= 0),"Colder than absolute zero!"
   return ((Temperature-273)*1.8)+32

print (KelvinToFahrenheit(273))
print (int(KelvinToFahrenheit(505.78)))
print (KelvinToFahrenheit(-5))
```

When the above code is executed, it produces the following result:

```
32.0
451
Traceback (most recent call last):
  File "test.py", line 9, in <module>
    print KelvinToFahrenheit(-5)
  File "test.py", line 4, in KelvinToFahrenheit
    assert (Temperature >= 0),"Colder than absolute zero!"
AssertionError: Colder than absolute zero!
```
