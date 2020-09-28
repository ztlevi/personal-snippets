# NUMPY

### count the occurance of items

```python
>>> a = numpy.array([0, 3, 0, 1, 0, 1, 2, 1, 0, 0, 0, 0, 1, 3, 4])
>>> unique, counts = numpy.unique(a, return_counts=True)
>>> dict(zip(unique, counts))
{0: 7, 1: 4, 2: 1, 3: 2, 4: 1}
```

## dict to numpy structured array

```python
import numpy as np
result = {0: 1.1181753789488595, 1: 0.5566080288678394, 2: 0.4718269778030734, 3: 0.48716683119447185, 4: 1.0, 5: 0.1395076201641266, 6: 0.20941558441558442}

names = ['id','data']
formats = ['f8','f8']
dtype = dict(names = names, formats=formats)
array = np.array(list(result.items()), dtype=dtype)

print(repr(array))
```

```
array([(0.0, 1.1181753789488595), (1.0, 0.5566080288678394),
       (2.0, 0.4718269778030734), (3.0, 0.48716683119447185), (4.0, 1.0),
       (5.0, 0.1395076201641266), (6.0, 0.20941558441558442)],
      dtype=[('id', '<f8'), ('data', '<f8')])
```

## Order

**order** : {‘C’,’F’, ‘A’, ‘K’}, optional

The elements of `a` are read using this index order. ‘C’ means to index the elements in row-major,
C-style order, with the last axis index changing fastest, back to the first axis index changing
slowest. ‘F’ means to index the elements in column-major, Fortran-style order, with the first index
changing fastest, and the last index changing slowest. Note that the ‘C’ and ‘F’ options take no
account of the memory layout of the underlying array, and only refer to the order of axis indexing.
‘A’ means to read the elements in Fortran-like index order if `a` is Fortran _contiguous_ in memory,
C-like order otherwise. ‘K’ means to read the elements in the order they occur in memory, except for
reversing the data when strides are negative. By default, ‘C’ index order is used.

![order](https://upload.wikimedia.org/wikipedia/commons/thumb/4/4d/Row_and_column_major_order.svg/340px-Row_and_column_major_order.svg.png)

## Arrays

**2D array**

```python
>>> x = np.array([[1, 2], [3, 4], [5, 6]])
>>> x[[0, 1, 2], [0, 1, 0]]
array([1, 4, 5])
```

**Axis**

```python
a = np.array([[1,2,3,1,2],[1,1,1,3,2]], np.int32)
print(np.sum(a, axis=0))
# [2 3 4 4 4]
print(np.sum(a, axis=1))
# [9 8]
```

A numpy array is a grid of values, all of the same type, and is indexed by a tuple of nonnegative
integers. The number of dimensions is the rank of the array; the shape of an array is a tuple of
integers giving the size of the array along each dimension.

We can initialize numpy arrays from nested Python lists, and access elements using square brackets:

```python
import numpy as np

a = np.array([1, 2, 3])   # Create a rank 1 array
print(type(a))            # Prints "<class 'numpy.ndarray'>"
print(a.shape)            # Prints "(3,)"
print(a[0], a[1], a[2])   # Prints "1 2 3"
a[0] = 5                  # Change an element of the array
print(a)                  # Prints "[5, 2, 3]"

b = np.array([[1,2,3],[4,5,6]])    # Create a rank 2 array
print(b.shape)                     # Prints "(2, 3)"
print(b[0, 0], b[0, 1], b[1, 0])   # Prints "1 2 4"
```

**Initialize array**: Numpy also provides many functions to create arrays:

```python
import numpy as np

a = np.zeros((2,2))   # Create an array of all zeros
print(a)              # Prints "[[ 0.  0.]
                      #          [ 0.  0.]]"

b = np.ones((1,2))    # Create an array of all ones
print(b)              # Prints "[[ 1.  1.]]"

c = np.full((2,2), 7)  # Create a constant array
print(c)               # Prints "[[ 7.  7.]
                       #          [ 7.  7.]]"

d = np.eye(2)         # Create a 2x2 identity matrix
print(d)              # Prints "[[ 1.  0.]
                      #          [ 0.  1.]]"

e = np.random.random((2,2))  # Create an array filled with random values
print(e)                     # Might print "[[ 0.91940167  0.08143941]
                             #               [ 0.68744134  0.87236687]]"
e = np.random.randn(2, 2) # Return a sample (or samples) from the “standard normal” distribution.
# [[-0.18404246  1.03765349  0.61357609]
# [ 0.40452669 -0.51612267 -0.20065114]
# [-1.35510832  0.80965974 -0.62247379]]
```

You can read about other methods of array creation in the documentation.

### Array indexing

Numpy offers several ways to index into arrays.

Array indexing will create array copies.

**Slicing**: Similar to Python lists, numpy arrays can be sliced. Since arrays may be
multidimensional, you must specify a slice for each dimension of the array:

```python
import numpy as np

# Create the following rank 2 array with shape (3, 4)
# [[ 1  2  3  4]
#  [ 5  6  7  8]
#  [ 9 10 11 12]]
a = np.array([[1,2,3,4], [5,6,7,8], [9,10,11,12]])

# Use slicing to pull out the subarray consisting of the first 2 rows
# and columns 1 and 2; b is the following array of shape (2, 2):
# [[2 3]
#  [6 7]]
b = a[:2, 1:3]

# A slice of an array is a view into the same data, so modifying it
# will modify the original array.
print(a[0, 1])   # Prints "2"
b[0, 0] = 77     # b[0, 0] is the same piece of data as a[0, 1]
print(a[0, 1])   # Prints "77"
```

You can also mix integer indexing with slice indexing. However, doing so will yield an array of
lower rank than the original array. Note that this is quite different from the way that MATLAB
handles array slicing:

```python
import numpy as np

# Create the following rank 2 array with shape (3, 4)
# [[ 1  2  3  4]
#  [ 5  6  7  8]
#  [ 9 10 11 12]]
a = np.array([[1,2,3,4], [5,6,7,8], [9,10,11,12]])

# Two ways of accessing the data in the middle row of the array.
# Mixing integer indexing with slices yields an array of lower rank,
# while using only slices yields an array of the same rank as the
# original array:
row_r1 = a[1, :]    # Rank 1 view of the second row of a
row_r2 = a[1:2, :]  # Rank 2 view of the second row of a
print(row_r1, row_r1.shape)  # Prints "[5 6 7 8] (4,)"
print(row_r2, row_r2.shape)  # Prints "[[5 6 7 8]] (1, 4)"

# We can make the same distinction when accessing columns of an array:
col_r1 = a[:, 1]
col_r2 = a[:, 1:2]
print(col_r1, col_r1.shape)  # Prints "[ 2  6 10] (3,)"
print(col_r2, col_r2.shape)  # Prints "[[ 2]
                             #          [ 6]
                             #          [10]] (3, 1)"
```

**Integer array indexing**: When you index into numpy arrays using slicing, the resulting array view
will always be a subarray of the original array. In contrast, integer array indexing allows you to
construct arbitrary arrays using the data from another array. Here is an example:

```python
import numpy as np

a = np.array([[1,2], [3, 4], [5, 6]])

# An example of integer array indexing.
# The returned array will have shape (3,) and
print(a[[0, 1, 2], [0, 1, 0]])  # Prints "[1 4 5]"

# The above example of integer array indexing is equivalent to this:
print(np.array([a[0, 0], a[1, 1], a[2, 0]]))  # Prints "[1 4 5]"

# When using integer array indexing, you can reuse the same
# element from the source array:
print(a[[0, 0], [1, 1]])  # Prints "[2 2]"

# Equivalent to the previous integer array indexing example
print(np.array([a[0, 1], a[0, 1]]))  # Prints "[2 2]"
```

One useful trick with integer array indexing is selecting or mutating one element from each row of a
matrix:

```python
import numpy as np

# Create a new array from which we will select elements
a = np.array([[1,2,3], [4,5,6], [7,8,9], [10, 11, 12]])

print(a)  # prints "array([[ 1,  2,  3],
          #                [ 4,  5,  6],
          #                [ 7,  8,  9],
          #                [10, 11, 12]])"

# Create an array of indices
b = np.array([0, 2, 0, 1])

# Select one element from each row of a using the indices in b
print(a[np.arange(4), b])  # Prints "[ 1  6  7 11]"

# Mutate one element from each row of a using the indices in b
a[np.arange(4), b] += 10

print(a)  # prints "array([[11,  2,  3],
          #                [ 4,  5, 16],
          #                [17,  8,  9],
          #                [10, 21, 12]])
```

**Boolean array indexing**: Boolean array indexing lets you pick out arbitrary elements of an array.
Frequently this type of indexing is used to select the elements of an array that satisfy some
condition. Here is an example:

```python
import numpy as np

a = np.array([[1,2], [3, 4], [5, 6]])

bool_idx = (a > 2)   # Find the elements of a that are bigger than 2;
                     # this returns a numpy array of Booleans of the same
                     # shape as a, where each slot of bool_idx tells
                     # whether that element of a is > 2.

print(bool_idx)      # Prints "[[False False]
                     #          [ True  True]
                     #          [ True  True]]"

# We use boolean array indexing to construct a rank 1 array
# consisting of the elements of a corresponding to the True values
# of bool_idx
print(a[bool_idx])  # Prints "[3 4 5 6]"

# We can do all of the above in a single concise statement:
print(a[a > 2])     # Prints "[3 4 5 6]"
```

For brevity we have left out a lot of details about numpy array indexing; if you want to know more
you should read the documentation.

**Shuffle indexing**:

```python
N = 10
nums = np.arange(N)
idxs = np.arange(N)
np.random.shuffle(idxs)
print("BEFORE...")
print(nums[0:N])
nums = nums[idxs]
print("AFTER...")
print(nums[0:N])
```

Output:

```
BEFORE...
[0 1 2 3 4 5 6 7 8 9]
AFTER...
[8 3 4 0 1 9 6 7 2 5]
```

**Using
[`ix_`](https://docs.scipy.org/doc/numpy/reference/generated/numpy.ix_.html#numpy.ix_ "numpy.ix_")
one can quickly construct index arrays that will index the cross product**

```python
M = np.arange(25).reshape((5, 5))
print(M)

idxes_top = np.ix_([2, 3, 4], range(5))
print(idxes_top)
print(M[idxes_top])

>>> [[ 0  1  2  3  4]
>>>  [ 5  6  7  8  9]
>>>  [10 11 12 13 14]
>>>  [15 16 17 18 19]
>>>  [20 21 22 23 24]]
>>> (array([[2],
>>>        [3],
>>>        [4]]), array([[0, 1, 2, 3, 4]]))
>>> [[10 11 12 13 14]
>>>  [15 16 17 18 19]
>>>  [20 21 22 23 24]]

>>> ixgrid[0].shape, ixgrid[1].shape
((2, 1), (1, 2))
>>> a[ixgrid]
array([[2, 4],
       [7, 9]])
```

### Datatypes

Every numpy array is a grid of elements of the same type. Numpy provides a large set of numeric
datatypes that you can use to construct arrays. Numpy tries to guess a datatype when you create an
array, but functions that construct arrays usually also include an optional argument to explicitly
specify the datatype. Here is an example:

```python
import numpy as np

x = np.array([1, 2])   # Let numpy choose the datatype
print(x.dtype)         # Prints "int64"

x = np.array([1.0, 2.0])   # Let numpy choose the datatype
print(x.dtype)             # Prints "float64"

x = np.array([1, 2], dtype=np.int64)   # Force a particular datatype
print(x.dtype)                         # Prints "int64"
```

You can read all about numpy datatypes in the documentation.

### Array math

Basic mathematical functions operate elementwise on arrays, and are available both as operator
overloads and as functions in the numpy module:

```python
import numpy as np

x = np.array([[1,2],[3,4]], dtype=np.float64)
y = np.array([[5,6],[7,8]], dtype=np.float64)

# Elementwise sum; both produce the array
# [[ 6.0  8.0]
#  [10.0 12.0]]
print(x + y)
print(np.add(x, y))

# Elementwise difference; both produce the array
# [[-4.0 -4.0]
#  [-4.0 -4.0]]
print(x - y)
print(np.subtract(x, y))

# Elementwise product; both produce the array
# [[ 5.0 12.0]
#  [21.0 32.0]]
print(x * y)
print(np.multiply(x, y))

# Elementwise division; both produce the array
# [[ 0.2         0.33333333]
#  [ 0.42857143  0.5       ]]
print(x / y)
print(np.divide(x, y))

# Elementwise square root; produces the array
# [[ 1.          1.41421356]
#  [ 1.73205081  2.        ]]
print(np.sqrt(x))
```

Note that unlike MATLAB, \* is elementwise multiplication, not matrix multiplication. We instead use
the dot function to compute inner products of vectors, to multiply a vector by a matrix, and to
multiply matrices. dot is available both as a function in the numpy module and as an instance method
of array objects:

```python
import numpy as np

x = np.array([[1,2],[3,4]])
y = np.array([[5,6],[7,8]])

v = np.array([9,10])
w = np.array([11, 12])

# Inner product of vectors; both produce 219
print(v.dot(w))
print(np.dot(v, w))

# Matrix / vector product; both produce the rank 1 array [29 67]
print(x.dot(v))
print(np.dot(x, v))

# Matrix / matrix product; both produce the rank 2 array
# [[19 22]
#  [43 50]]
print(x.dot(y))
print(np.dot(x, y))
```

Numpy provides many useful functions for performing computations on arrays; one of the most useful
is sum:

```python
import numpy as np

x = np.array([[1,2],[3,4]])

print(np.sum(x))  # Compute sum of all elements; prints "10"
print(np.sum(x, axis=0))  # Compute sum of each column; prints "[4 6]"
print(np.sum(x, axis=1))  # Compute sum of each row; prints "[3 7]"
```

You can find the full list of mathematical functions provided by numpy in the documentation.

Apart from computing mathematical functions using arrays, we frequently need to reshape or otherwise
manipulate data in arrays. The simplest example of this type of operation is transposing a matrix;
to transpose a matrix, simply use the T attribute of an array object:

```python
import numpy as np

x = np.array([[1,2], [3,4]])
print(x)    # Prints "[[1 2]
            #          [3 4]]"
print(x.T)  # Prints "[[1 3]
            #          [2 4]]"

# Note that taking the transpose of a rank 1 array does nothing:
v = np.array([1,2,3])
print(v)    # Prints "[1 2 3]"
print(v.T)  # Prints "[1 2 3]"
```

## General Broadcasting Rules[¶](https://docs.scipy.org/doc/numpy-1.13.0/user/basics.broadcasting.html#general-broadcasting-rules)

When operating on two arrays, NumPy compares their shapes element-wise. It starts with the trailing
dimensions, and works its way forward. Two dimensions are compatible when

1.  they are equal, or
2.  one of them is 1

If these conditions are not met, a `ValueError: frames are not aligned` exception is thrown,
indicating that the arrays have incompatible shapes. The size of the resulting array is the maximum
size along each dimension of the input arrays.

Arrays do not need to have the same _number_ of dimensions. For example, if you have a `256x256x3`
array of RGB values, and you want to scale each color in the image by a different value, you can
multiply the image by a one-dimensional array with 3 values. Lining up the sizes of the trailing
axes of these arrays according to the broadcast rules, shows that they are compatible:

```python
Image(3d array): 256 x 256 x 3
Scale  (1d array):             3
Result (3d array): 256 x 256 x 3
```

When either of the dimensions compared is one, the other is used. In other words, dimensions with
size 1 are stretched or “copied” to match the other.

In the following example, both the `A` and `B` arrays have axes with length one that are expanded to
a larger size during the broadcast operation:

```python
A(4darray):  8 x 1 x 6 x 1
B      (3d array):      7 x 1 x 5
Result (4d array):  8 x 7 x 6 x 5
```

Here are some more examples:

```python
A(2d array):5 x4
B(1darray):1
Result (2d array):  5 x 4

A      (2d array):  5 x 4
B      (1d array):      4
Result (2d array):  5 x 4

A      (3d array):  15 x 3 x 5
B      (3d array):  15 x 1 x 5
Result (3d array):  15 x 3 x 5

A      (3d array):  15 x 3 x 5
B      (2d array):       3 x 5
Result (3d array):  15 x 3 x 5

A      (3d array):  15 x 3 x 5
B      (2d array):       3 x 1
Result (3d array):  15 x 3 x 5
```

Here are examples of shapes that do not broadcast:

```python
A      (1d array):  3
B      (1d array):  4 \# trailing dimensions do not match

A      (2d array):      2 x 1
B      (3d array):  8 x 4 x 3 \# second from last dimensions mismatched
```

## newaxis

```python
x = np.array([[0, 1, 2],
              [3, 4, 5],
              [6, 7, 8],
              [9, 10, 11]])

rows = np.array([0, 3], dtype=np.intp)
columns = np.array([0, 2], dtype=np.intp)
print(rows[:, np.newaxis])
# array([[0],
#        [3]])
print(x[rows[:, np.newaxis], columns])
# array([[ 0,  2],
#        [ 9, 11]])
print(x[[[0, 0], [3,3]], [[0, 2], [0,2]]]) # same
```

(5000, 3072) -> (5000, 1, 3072) (5000, 1, 3072) - (500, 3072) -> (5000, 500, 3072)

## flatnonzero

Return indices that are non-zero in the flattened version of a.

This is equivalent to `a.ravel().nonzero()[0]`.

```python
>>> np.flatnonzero(np.array([-2, -5,  0,  1,  2]))
[0, 1, 3, 4]

>>> np.flatnonzero(np.array([False, False, True, True, False, True]))
[2, 3, 5]
```

## ravel

```python
>>> x = np.array([[1, 2, 3], [4, 5, 6]])
>>> print np.ravel(x)
[1 2 3 4 5 6]
```

```python
>>> print x.reshape(-1)
[1 2 3 4 5 6]
```

```python
>>> print np.ravel(x, order='F')
[1 4 2 5 3 6]
```

## nonzero

```python
>>> x = np.array([[1,0,0], [0,2,0], [1,1,0]])
>>> x
array([[1, 0, 0],
       [0, 2, 0],
       [1, 1, 0]])
>>> np.nonzero(x)
(array([0, 1, 2, 2]), array([0, 1, 0, 1]))
```

## Frobenius norm:

```python
# There are many ways to decide whether
# two matrices are similar; one of the simplest is the Frobenius norm. In case
# you haven't seen it before, the Frobenius norm of two matrices is the square
# root of the squared sum of differences of all elements; in other words, reshape
# the matrices into vectors and compute the Euclidean distance between them.
difference = np.linalg.norm(dists - dists_one, ord='fro')
```

```python
    diff = X - self.X_train[:, np.newaxis] # MemoryError!
    dists = np.sqrt((diff ** 2).sum(axis=2))
```

## Compute L2 Distance

```python
dists = np.sqrt((X**2).sum(axis=1, keepdims=True) + (self.X_train**2).sum(axis=1) - 2 * X.dot(self.X_train.T))
```

## Declare a structured array with x and y coordinates covering the [0,1]x[0,1] area.

```python
Z = np.zeros((10,10), [('x',float),('y',float)])
Z['x'], Z['y'] = np.meshgrid(np.linspace(0,1,10),
                             np.linspace(0,1,10))
```
