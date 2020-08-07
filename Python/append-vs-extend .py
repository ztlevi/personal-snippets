#append: Appends object at end.
 
x = [1, 2, 3]
x.append([4, 5])
print (x)
#gives you: [1, 2, 3, [4, 5]]
 
#extend: Extends list by appending elements from the iterable.
 
x = [1, 2, 3]
x.extend([4, 5])
print (x)
#gives you: [1, 2, 3, 4, 5]