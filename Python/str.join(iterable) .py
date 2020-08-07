Return a string which is the concatenation of the strings in the iterable iterable. A TypeError will be raised if there are any non-string values in iterable, including bytes objects. The separator between elements is the string providing this method.
---

["wrt","wrf","er","ett","rftt"]
chars = set("".join(words))
# chars = {'e', 'f', 't', 'r', 'w'}
