Iterate through a python dictionary
**kwargs is considered as a dictionary
---

# key is just a variable name.
 
for key in d:
# will simply loop over the keys in the dictionary, rather than the keys and values. To loop over both key and value you can use the following:
 
# For Python 2.x:
for key, value in d.iteritems():
 
# For Python 3.x:
for key, value in d.items():
 