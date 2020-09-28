# Namedtuple

https://www.geeksforgeeks.org/namedtuple-in-python/

Python supports a type of container like
[dictionaries](http://quiz.geeksforgeeks.org/python-set-4-dictionary-keywords-python/) called
“**namedtuples()**” present in module, “**collection**“. Like dictionaries they contain keys that
are hashed to a particular value. But on contrary, it supports both access from key value and
iteration, the functionality that dictionaries lack.

**Operations on namedtuple() :**

**Access Operations**

**1. Access by index :** The attribute values of namedtuple() are ordered and can be accessed using
the index number unlike dictionaries which are not accessible by index.

**2. Access by keyname :** Access by keyname is also allowed as in dictionaries.

**3. using getattr() :-** This is yet another way to access the value by giving namedtuple and key
value as its argument.

```
# Python code to demonstrate namedtuple() and
# Access by name, index and getattr()

# importing "collections" for namedtuple()
import collections

# Declaring namedtuple()
Student = collections.namedtuple('Student',['name','age','DOB'])

# Adding values
S = Student('Nandini','19','2541997')

# Access using index
print ("The Student age using index is : ",end ="")
print (S[1])

# Access using name
print ("The Student name using keyname is : ",end ="")
print (S.name)

# Access using getattr()
print ("The Student DOB using getattr() is : ",end ="")
print (getattr(S,'DOB'))
```

Output :

```
The Student age using index is : 19
The Student name using keyname is : Nandini
The Student DOB using getattr() is : 2541997
```

**Conversion Operations**

**1. \_make() :-** This function is used to return a **namedtuple() from the iterable** passed as
argument.

**2. \_asdict() :-** This function returns **the
[OrdereDict()](https://www.geeksforgeeks.org/ordereddict-in-python/)** as constructed from the
mapped values of namedtuple().

**3. using “**” (double star) operator** :- This function is used to **convert a dictionary into the
namedtuple().\*\*

```
# Python code to demonstrate namedtuple() and
# _make(), _asdict() and "**" operator

# importing "collections" for namedtuple()
import collections

# Declaring namedtuple()
Student = collections.namedtuple('Student',['name','age','DOB'])

# Adding values
S = Student('Nandini','19','2541997')

# initializing iterable
li = ['Manjeet', '19', '411997' ]

# initializing dict
di = { 'name' : "Nikhil", 'age' : 19 , 'DOB' : '1391997' }

# using _make() to return namedtuple()
print ("The namedtuple instance using iterable is  : ")
print (Student._make(li))

# using _asdict() to return an OrderedDict()
print ("The OrderedDict instance using namedtuple is  : ")
print (S._asdict())

# using ** operator to return namedtuple from dictionary
print ("The namedtuple instance from dict is  : ")
print (Student(**di))

```

Output :

```
The namedtuple instance using iterable is  :
Student(name='Manjeet', age='19', DOB='411997')
The OrderedDict instance using namedtuple is  :
OrderedDict([('name', 'Nandini'), ('age', '19'), ('DOB', '2541997')])
The namedtuple instance from dict is  :
Student(name='Nikhil', age=19, DOB='1391997')
```

**Additional Operations**

---

**1. \_fields :-** This function is used to return **all the keynames** of the namespace declared.

**2. \_replace() :-** This function is used to **change the values** mapped with the passed keyname.

```
# Python code to demonstrate namedtuple() and
# _fields and _replace()

# importing "collections" for namedtuple()
import collections

# Declaring namedtuple()
Student = collections.namedtuple('Student',['name','age','DOB'])

# Adding values
S = Student('Nandini','19','2541997')

# using _fields to display all the keynames of namedtuple()
print ("All the fields of students are : ")
print (S._fields)

# using _replace() to change the attribute values of namedtuple
print ("The modified namedtuple is : ")
print(S._replace(name = 'Manjeet'))

```

Output :

```

All the fields of students are :
('name', 'age', 'DOB')
The modified namedtuple is :
Student(name='Manjeet', age='19', DOB='2541997')
```

This article is contributed by
**[Manjeet Singh](https://auth.geeksforgeeks.org/profile.php?user=manjeet_04&list=practice)**. If
you like GeeksforGeeks and would like to contribute, you can also write an article using
[contribute.geeksforgeeks.org](http://www.contribute.geeksforgeeks.org/) or mail your article to
contribute@geeksforgeeks.org. See your article appearing on the GeeksforGeeks main page and help
other Geeks.

Please write comments if you find anything incorrect, or you want to share more information about
the topic discussed above.
