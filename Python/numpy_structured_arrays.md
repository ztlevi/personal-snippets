# Structured arrays[¶](https://docs.scipy.org/doc/numpy/user/basics.rec.html#module-numpy.doc.structured_arrays "Permalink to this headline")

https://docs.scipy.org/doc/numpy/user/basics.rec.html

## Introduction[¶](https://docs.scipy.org/doc/numpy/user/basics.rec.html#introduction "Permalink to this headline")

Structured arrays are ndarrays whose datatype is a composition of simpler datatypes organized as a
sequence of named [fields](https://docs.scipy.org/doc/numpy/glossary.html#term-field). For example,

```python
>>> x = np.array([('Rex', 9, 81.0), ('Fido', 3, 27.0)],
...              dtype=[('name', 'U10'), ('age', 'i4'), ('weight', 'f4')])
>>> x
array([('Rex', 9, 81.0), ('Fido', 3, 27.0)],
      dtype=[('name', 'S10'), ('age', '<i4'), ('weight', '<f4')])
```

Here `x` is a one-dimensional array of length two whose datatype is a structure with three
fields: 1. A string of length 10 or less named ‘name’, 2. a 32-bit integer named ‘age’, and 3. a
32-bit float named ‘weight’.

If you index `x` at position 1 you get a structure:

```python
>>> x[1]
('Fido', 3, 27.0)
```

You can access and modify individual fields of a structured array by indexing with the field name:

```python
>>> x['age']
array([9, 3], dtype=int32)
>>> x['age'] = 5
>>> x
array([('Rex', 5, 81.0), ('Fido', 5, 27.0)],
      dtype=[('name', 'S10'), ('age', '<i4'), ('weight', '<f4')])
```

Structured datatypes are designed to be able to mimic ‘structs’ in the C language, and share a
similar memory layout. They are meant for interfacing with C code and for low-level manipulation of
structured buffers, for example for interpreting binary blobs. For these purposes they support
specialized features such as subarrays, nested datatypes, and unions, and allow control over the
memory layout of the structure.

Users looking to manipulate tabular data, such as stored in csv files, may find other pydata
projects more suitable, such as xarray, pandas, or DataArray. These provide a high-level interface
for tabular data analysis and are better optimized for that use. For instance, the C-struct-like
memory layout of structured arrays in numpy can lead to poor cache behavior in comparison.

## Structured Datatypes[¶](https://docs.scipy.org/doc/numpy/user/basics.rec.html#structured-datatypes "Permalink to this headline")

A structured datatype can be thought of as a sequence of bytes of a certain length (the structure’s
itemsize) which is interpreted as a collection of fields. Each field has a name, a datatype, and a
byte offset within the structure. The datatype of a field may be any numpy datatype including other
structured datatypes, and it may also be a sub-array which behaves like an ndarray of a specified
shape. The offsets of the fields are arbitrary, and fields may even overlap. These offsets are
usually determined automatically by numpy, but can also be specified.

### Structured Datatype Creation[¶](https://docs.scipy.org/doc/numpy/user/basics.rec.html#structured-datatype-creation "Permalink to this headline")

Structured datatypes may be created using the function
[`numpy.dtype`](https://docs.scipy.org/doc/numpy/reference/generated/numpy.dtype.html#numpy.dtype "numpy.dtype").
There are 4 alternative forms of specification which vary in flexibility and conciseness. These are
further documented in the
[Data Type Objects](https://docs.scipy.org/doc/numpy/reference/arrays.dtypes.html#arrays-dtypes-constructing)
reference page, and in summary they are:

1.  A list of tuples, one tuple per field

    Each tuple has the form `(fieldname, datatype, shape)` where shape is optional. `fieldname` is a
    string (or tuple if titles are used, see
    [Field Titles](https://docs.scipy.org/doc/numpy/user/basics.rec.html#titles) below), `datatype`
    may be any object convertible to a datatype, and `shape` is a tuple of integers specifying
    subarray shape.

    ```python
    >>> np.dtype([('x', 'f4'), ('y', np.float32), ('z', 'f4', (2,2))])
    dtype=[('x', '<f4'), ('y', '<f4'), ('z', '<f4', (2, 2))])
    ```

    If `fieldname` is the empty string `''`, the field will be given a default name of the form
    `f#`, where `\#` is the integer index of the field, counting from 0 from the left:

    ```python
    >>> np.dtype([('x', 'f4'),('', 'i4'),('z', 'i8')])
    dtype([('x', '<f4'), ('f1', '<i4'), ('z', '<i8')])
    ```

    The byte offsets of the fields within the structure and the total structure itemsize are
    determined automatically.

2.  A string of comma-separated dtype specifications

    In this shorthand notation any of the
    [string dtype specifications](https://docs.scipy.org/doc/numpy/reference/arrays.dtypes.html#arrays-dtypes-constructing)
    may be used in a string and separated by commas. The itemsize and byte offsets of the fields are
    determined automatically, and the field names are given the default names `f0`, `f1`, etc.

    ```python
    >>> np.dtype('i8,f4,S3')
    dtype([('f0', '<i8'), ('f1', '<f4'), ('f2', 'S3')])
    >>> np.dtype('3int8, float32, (2,3)float64')
    dtype([('f0', 'i1', 3), ('f1', '<f4'), ('f2', '<f8', (2, 3))])
    ```

3.  A dictionary of field parameter arrays

    This is the most flexible form of specification since it allows control over the byte-offsets of
    the fields and the itemsize of the structure.

    The dictionary has two required keys, ‘names’ and ‘formats’, and four optional keys, ‘offsets’,
    ‘itemsize’, ‘aligned’ and ‘titles’. The values for ‘names’ and ‘formats’ should respectively be
    a list of field names and a list of dtype specifications, of the same length. The optional
    ‘offsets’ value should be a list of integer byte-offsets, one for each field within the
    structure. If ‘offsets’ is not given the offsets are determined automatically. The optional
    ‘itemsize’ value should be an integer describing the total size in bytes of the dtype, which
    must be large enough to contain all the fields.

    ```python
    >>> np.dtype({'names': ['col1', 'col2'], 'formats': ['i4','f4']})
    dtype([('col1', '<i4'), ('col2', '<f4')])
    >>> np.dtype({'names': ['col1', 'col2'],
    ...           'formats': ['i4','f4'],
    ...           'offsets': [0, 4],
    ...           'itemsize': 12})
    dtype({'names':['col1','col2'], 'formats':['<i4','<f4'], 'offsets':[0,4], 'itemsize':12})
    ```

    Offsets may be chosen such that the fields overlap, though this will mean that assigning to one
    field may clobber any overlapping field’s data. As an exception, fields of `numpy.object` type
    cannot overlap with other fields, because of the risk of clobbering the internal object pointer
    and then dereferencing it.

    The optional ‘aligned’ value can be set to `True` to make the automatic offset computation use
    aligned offsets (see
    [Automatic Byte Offsets and Alignment](https://docs.scipy.org/doc/numpy/user/basics.rec.html#offsets-and-alignment)),
    as if the ‘align’ keyword argument of
    [`numpy.dtype`](https://docs.scipy.org/doc/numpy/reference/generated/numpy.dtype.html#numpy.dtype "numpy.dtype")
    had been set to True.

    The optional ‘titles’ value should be a list of titles of the same length as ‘names’, see
    [Field Titles](https://docs.scipy.org/doc/numpy/user/basics.rec.html#titles) below.

4.  A dictionary of field names

    The use of this form of specification is discouraged, but documented here because older numpy
    code may use it. The keys of the dictionary are the field names and the values are tuples
    specifying type and offset:

    ```python
    >>> np.dtype=({'col1': ('i1',0), 'col2': ('f4',1)})
    dtype([(('col1'), 'i1'), (('col2'), '>f4')])
    ```

    This form is discouraged because Python dictionaries do not preserve order in Python versions
    before Python 3.6, and the order of the fields in a structured dtype has meaning.
    [Field Titles](https://docs.scipy.org/doc/numpy/user/basics.rec.html#titles) may be specified by
    using a 3-tuple, see below.

### Manipulating and Displaying Structured Datatypes[¶](https://docs.scipy.org/doc/numpy/user/basics.rec.html#manipulating-and-displaying-structured-datatypes "Permalink to this headline")

The list of field names of a structured datatype can be found in the `names` attribute of the dtype
object:

```python
>>> d = np.dtype([('x', 'i8'), ('y', 'f4')])
>>> d.names
('x', 'y')
```

The field names may be modified by assigning to the `names` attribute using a sequence of strings of
the same length.

The dtype object also has a dictionary-like attribute, `fields`, whose keys are the field names (and
[Field Titles](https://docs.scipy.org/doc/numpy/user/basics.rec.html#titles), see below) and whose
values are tuples containing the dtype and byte offset of each field.

```python
>>> d.fields
mappingproxy({'x': (dtype('int64'), 0), 'y': (dtype('float32'), 8)})
```

Both the `names` and `fields` attributes will equal `None` for unstructured arrays. The recommended
way to test if a dtype is structured is with _if dt.names is not None_ rather than _if dt.names_, to
account for dtypes with 0 fields.

The string representation of a structured datatype is shown in the “list of tuples” form if
possible, otherwise numpy falls back to using the more general dictionary form.

### Automatic Byte Offsets and Alignment[¶](https://docs.scipy.org/doc/numpy/user/basics.rec.html#automatic-byte-offsets-and-alignment "Permalink to this headline")

Numpy uses one of two methods to automatically determine the field byte offsets and the overall
itemsize of a structured datatype, depending on whether `align=True` was specified as a keyword
argument to
[`numpy.dtype`](https://docs.scipy.org/doc/numpy/reference/generated/numpy.dtype.html#numpy.dtype "numpy.dtype").

By default (`align=False`), numpy will pack the fields together such that each field starts at the
byte offset the previous field ended, and the fields are contiguous in memory.

```python
>>> defprint_offsets(d):
...     print("offsets:", [d.fields[name][1] for name in d.names])
...     print("itemsize:", d.itemsize)
>>> print_offsets(np.dtype('u1,u1,i4,u1,i8,u2'))
offsets: [0, 1, 2, 6, 7, 15]
itemsize: 17
```

If `align=True` is set, numpy will pad the structure in the same way many C compilers would pad a
C-struct. Aligned structures can give a performance improvement in some cases, at the cost of
increased datatype size. Padding bytes are inserted between fields such that each field’s byte
offset will be a multiple of that field’s alignment, which is usually equal to the field’s size in
bytes for simple datatypes, see
[`PyArray_Descr.alignment`](https://docs.scipy.org/doc/numpy/reference/c-api.types-and-structures.html#c.PyArray_Descr.alignment "PyArray_Descr.alignment").
The structure will also have trailing padding added so that its itemsize is a multiple of the
largest field’s alignment.

```python
>>> print_offsets(np.dtype('u1,u1,i4,u1,i8,u2', align=True))
offsets: [0, 1, 4, 8, 16, 24]
itemsize: 32
```

Note that although almost all modern C compilers pad in this way by default, padding in C structs is
C-implementation-dependent so this memory layout is not guaranteed to exactly match that of a
corresponding struct in a C program. Some work may be needed, either on the numpy side or the C
side, to obtain exact correspondence.

If offsets were specified using the optional `offsets` key in the dictionary-based dtype
specification, setting `align=True` will check that each field’s offset is a multiple of its size
and that the itemsize is a multiple of the largest field size, and raise an exception if not.

If the offsets of the fields and itemsize of a structured array satisfy the alignment conditions,
the array will have the `ALIGNED` flag set.

A convenience function `numpy.lib.recfunctions.repack_fields` converts an aligned dtype or array to
a packed one and vice versa. It takes either a dtype or structured ndarray as an argument, and
returns a copy with fields re-packed, with or without padding bytes.

### Field Titles[¶](https://docs.scipy.org/doc/numpy/user/basics.rec.html#field-titles "Permalink to this headline")

In addition to field names, fields may also have an associated title, an alternate name, which is
sometimes used as an additional description or alias for the field. The title may be used to index
an array, just like a field name.

To add titles when using the list-of-tuples form of dtype specification, the field name may be
specified as a tuple of two strings instead of a single string, which will be the field’s title and
field name respectively. For example:

```python
>>> np.dtype([(('my title', 'name'), 'f4')])
```

When using the first form of dictionary-based specification, the titles may be supplied as an extra
`'titles'` key as described above. When using the second (discouraged) dictionary-based
specification, the title can be supplied by providing a 3-element tuple `(datatype, offset, title)`
instead of the usual 2-element tuple:

```python
>>> np.dtype({'name': ('i4', 0, 'my title')})
```

The `dtype.fields` dictionary will contain titles as keys, if any titles are used. This means
effectively that a field with a title will be represented twice in the fields dictionary. The tuple
values for these fields will also have a third element, the field title. Because of this, and
because the `names` attribute preserves the field order while the `fields` attribute may not, it is
recommended to iterate through the fields of a dtype using the `names` attribute of the dtype, which
will not list titles, as in:

```python
>>> for name in d.names:
...     print(d.fields[name][:2])
```

### Union types[¶](https://docs.scipy.org/doc/numpy/user/basics.rec.html#union-types "Permalink to this headline")

Structured datatypes are implemented in numpy to have base type `numpy.void` by default, but it is
possible to interpret other numpy types as structured types using the `(base_dtype, dtype)` form of
dtype specification described in
[Data Type Objects](https://docs.scipy.org/doc/numpy/reference/arrays.dtypes.html#arrays-dtypes-constructing).
Here, `base_dtype` is the desired underlying dtype, and fields and flags will be copied from
`dtype`. This dtype is similar to a ‘union’ in C.

## Indexing and Assignment to Structured arrays[¶](https://docs.scipy.org/doc/numpy/user/basics.rec.html#indexing-and-assignment-to-structured-arrays "Permalink to this headline")

### Assigning data to a Structured Array[¶](https://docs.scipy.org/doc/numpy/user/basics.rec.html#assigning-data-to-a-structured-array "Permalink to this headline")

There are a number of ways to assign values to a structured array: Using python tuples, using scalar
values, or using other structured arrays.

#### Assignment from Python Native Types (Tuples)[¶](https://docs.scipy.org/doc/numpy/user/basics.rec.html#assignment-from-python-native-types-tuples "Permalink to this headline")

The simplest way to assign values to a structured array is using python tuples. Each assigned value
should be a tuple of length equal to the number of fields in the array, and not a list or array as
these will trigger numpy’s broadcasting rules. The tuple’s elements are assigned to the successive
fields of the array, from left to right:

```python
>>> x = np.array([(1,2,3),(4,5,6)], dtype='i8,f4,f8')
>>> x[1] = (7,8,9)
>>> x
array([(1, 2., 3.), (7, 8., 9.)],
     dtype=[('f0', '<i8'), ('f1', '<f4'), ('f2', '<f8')])
```

#### Assignment from Scalars[¶](https://docs.scipy.org/doc/numpy/user/basics.rec.html#assignment-from-scalars "Permalink to this headline")

A scalar assigned to a structured element will be assigned to all fields. This happens when a scalar
is assigned to a structured array, or when an unstructured array is assigned to a structured array:

```python
>>> x = np.zeros(2, dtype='i8,f4,?,S1')
>>> x[:] = 3
>>> x
array([(3, 3.0, True, b'3'), (3, 3.0, True, b'3')],
      dtype=[('f0', '<i8'), ('f1', '<f4'), ('f2', '?'), ('f3', 'S1')])
>>> x[:] = np.arange(2)
>>> x
array([(0, 0.0, False, b'0'), (1, 1.0, True, b'1')],
      dtype=[('f0', '<i8'), ('f1', '<f4'), ('f2', '?'), ('f3', 'S1')])
```

Structured arrays can also be assigned to unstructured arrays, but only if the structured datatype
has just a single field:

```python
>>> twofield= np.zeros(2, dtype=[('A', 'i4'), ('B', 'i4')])
>>> onefield = np.zeros(2, dtype=[('A', 'i4')])
>>> nostruct = np.zeros(2, dtype='i4')
>>> nostruct[:] = twofield
ValueError: Can't cast from structure to non-structure, except if the structure only has a single field.
>>> nostruct[:] = onefield
>>> nostruct
array([0, 0], dtype=int32)
```

#### Assignment from other Structured Arrays[¶](https://docs.scipy.org/doc/numpy/user/basics.rec.html#assignment-from-other-structured-arrays "Permalink to this headline")

Assignment between two structured arrays occurs as if the source elements had been converted to
tuples and then assigned to the destination elements. That is, the first field of the source array
is assigned to the first field of the destination array, and the second field likewise, and so on,
regardless of field names. Structured arrays with a different number of fields cannot be assigned to
each other. Bytes of the destination structure which are not included in any of the fields are
unaffected.

```python
>>> a = np.zeros(3, dtype=[('a', 'i8'), ('b', 'f4'), ('c', 'S3')])
>>> b = np.ones(3, dtype=[('x', 'f4'), ('y', 'S3'), ('z', 'O')])
>>> b[:] = a
>>> b
array([(0.0, b'0.0', b''), (0.0, b'0.0', b''), (0.0, b'0.0', b'')],
      dtype=[('x', '<f4'), ('y', 'S3'), ('z', 'O')])
```

#### Assignment involving subarrays[¶](https://docs.scipy.org/doc/numpy/user/basics.rec.html#assignment-involving-subarrays "Permalink to this headline")

When assigning to fields which are subarrays, the assigned value will first be broadcast to the
shape of the subarray.

### Indexing Structured Arrays[¶](https://docs.scipy.org/doc/numpy/user/basics.rec.html#indexing-structured-arrays "Permalink to this headline")

#### Accessing Individual Fields[¶](https://docs.scipy.org/doc/numpy/user/basics.rec.html#accessing-individual-fields "Permalink to this headline")

Individual fields of a structured array may be accessed and modified by indexing the array with the
field name.

```python
>>> x= np.array([(1,2),(3,4)], dtype=[('foo', 'i8'), ('bar', 'f4')])
>>> x['foo']
array([1, 3])
>>> x['foo'] = 10
>>> x
array([(10, 2.), (10, 4.)],
      dtype=[('foo', '<i8'), ('bar', '<f4')])
```

The resulting array is a view into the original array. It shares the same memory locations and
writing to the view will modify the original array.

```python
>>> y = x['bar']
>>> y[:] = 10
>>> x
array([(10, 5.), (10, 5.)],
      dtype=[('foo', '<i8'), ('bar', '<f4')])
```

This view has the same dtype and itemsize as the indexed field, so it is typically a non-structured
array, except in the case of nested structures.

```python
>>> y.dtype, y.shape, y.strides
(dtype('float32'), (2,), (12,))
```

If the accessed field is a subarray, the dimensions of the subarray are appended to the shape of the
result:

```python
>>> x = np.zeros((2,2), dtype=[('a', np.int32), ('b', np.float64, (3,3))])
>>> x['a'].shape
(2, 2)
>>> x['b'].shape
(2, 2, 3, 3)
```

#### Accessing Multiple Fields[¶](https://docs.scipy.org/doc/numpy/user/basics.rec.html#accessing-multiple-fields "Permalink to this headline")

One can index and assign to a structured array with a multi-field index, where the index is a list
of field names.

Warning

The behavior of multi-field indexes changed from Numpy 1.15 to Numpy 1.16.

The result of indexing with a multi-field index is a view into the original array, as follows:

```python
>>> a = np.zeros(3, dtype=[('a', 'i4'), ('b', 'i4'), ('c', 'f4')])
>>> a[['a', 'c']]
array([(0, 0.), (0, 0.), (0, 0.)],
     dtype={'names':['a','c'], 'formats':['<i4','<f4'], 'offsets':[0,8], 'itemsize':12})
```

Assignment to the view modifies the original array. The view’s fields will be in the order they were
indexed. Note that unlike for single-field indexing, the view’s dtype has the same itemsize as the
original array, and has fields at the same offsets as in the original array, and unindexed fields
are merely missing.

Warning

In Numpy 1.15, indexing an array with a multi-field index returned a copy of the result above, but
with fields packed together in memory as if passed through `numpy.lib.recfunctions.repack_fields`.

The new behavior as of Numpy 1.16 leads to extra “padding” bytes at the location of unindexed fields
compared to 1.15. You will need to update any code which depends on the data having a “packed”
layout. For instance code such as:

```python
>>> a = np.zeros(3, dtype=[('a', 'i4'), ('b', 'i4'), ('c', 'f4')])
>>> a[['a','c']].view('i8')  # Fails in Numpy 1.16
ValueError: When changing to a smaller dtype, its size must be a divisor of the size of original dtype
```

will need to be changed. This code has raised a `FutureWarning` since Numpy 1.12, and similar code
has raised `FutureWarning` since 1.7.

In 1.16 a number of functions have been introduced in the
[:module:`numpy.lib.recfunctions`](https://docs.scipy.org/doc/numpy/user/basics.rec.html#id2) module
to help users account for this change. These are `numpy.lib.recfunctions.repack_fields`.
`numpy.lib.recfunctions.structured_to_unstructured`,
`numpy.lib.recfunctions.unstructured_to_structured`, `numpy.lib.recfunctions.apply_along_fields`,
`numpy.lib.recfunctions.assign_fields_by_name`, and `numpy.lib.recfunctions.require_fields`.

The function `numpy.lib.recfunctions.repack_fields` can always be used to reproduce the old
behavior, as it will return a packed copy of the structured array. The code above, for example, can
be replaced with:

```python
>>> repack_fields(a[['a','c']]).view('i8')  # supported in 1.16
array([0, 0, 0])
```

Furthermore, numpy now provides a new function `numpy.lib.recfunctions.structured_to_unstructured`
which is a safer and more efficient alternative for users who wish to convert structured arrays to
unstructured arrays, as the view above is often indeded to do. This function allows safe conversion
to an unstructured type taking into account padding, often avoids a copy, and also casts the
datatypes as needed, unlike the view. Code such as:

```python
>>> a = np.zeros(3, dtype=[('x', 'f4'), ('y', 'f4'), ('z', 'f4')])
>>> a[['x', 'z']].view('f4')
```

can be made safer by replacing with:

```python
>>> structured_to_unstructured(a[['x', 'z']])
array([0, 0, 0])
```

Assignment to an array with a multi-field index modifies the original array:

```python
>>> a[['a', 'c']] = (2, 3)
>>> a
array([(2, 0, 3.0), (2, 0, 3.0), (2, 0, 3.0)],
      dtype=[('a', '<i8'), ('b', '<i4'), ('c', '<f8')])
```

This obeys the structured array assignment rules described above. For example, this means that one
can swap the values of two fields using appropriate multi-field indexes:

```python
>>> a[['a', 'c']] = a[['c', 'a']]
```

#### Indexing with an Integer to get a Structured Scalar[¶](https://docs.scipy.org/doc/numpy/user/basics.rec.html#indexing-with-an-integer-to-get-a-structured-scalar "Permalink to this headline")

Indexing a single element of a structured array (with an integer index) returns a structured scalar:

```python
>>> x = np.array([(1, 2., 3.)], dtype='i,f,f')
>>> scalar = x[0]
>>> scalar
(1, 2., 3.)
>>> type(scalar)
numpy.void
```

Unlike other numpy scalars, structured scalars are mutable and act like views into the original
array, such that modifying the scalar will modify the original array. Structured scalars also
support access and assignment by field name:

```python
>>> x= np.array([(1,2),(3,4)], dtype=[('foo', 'i8'), ('bar', 'f4')])
>>> s = x[0]
>>> s['bar'] = 100
>>> x
array([(1, 100.), (3, 4.)],
      dtype=[('foo', '<i8'), ('bar', '<f4')])
```

Similarly to tuples, structured scalars can also be indexed with an integer:

```python
>>> scalar= np.array([(1, 2., 3.)], dtype='i,f,f')[0]
>>> scalar[0]
1
>>> scalar[1] = 4
```

Thus, tuples might be thought of as the native Python equivalent to numpy’s structured types, much
like native python integers are the equivalent to numpy’s integer types. Structured scalars may be
converted to a tuple by calling `ndarray.item`:

```python
>>> scalar.item(), type(scalar.item())
((1, 2.0, 3.0), tuple)
```

### Viewing Structured Arrays Containing Objects[¶](https://docs.scipy.org/doc/numpy/user/basics.rec.html#viewing-structured-arrays-containing-objects "Permalink to this headline")

In order to prevent clobbering object pointers in fields of `numpy.object` type, numpy currently
does not allow views of structured arrays containing objects.

### Structure Comparison[¶](https://docs.scipy.org/doc/numpy/user/basics.rec.html#structure-comparison "Permalink to this headline")

If the dtypes of two void structured arrays are equal, testing the equality of the arrays will
result in a boolean array with the dimensions of the original arrays, with elements set to `True`
where all fields of the corresponding structures are equal. Structured dtypes are equal if the field
names, dtypes and titles are the same, ignoring endianness, and the fields are in the same order:

```python
>>> a= np.zeros(2, dtype=[('a', 'i4'), ('b', 'i4')])
>>> b = np.ones(2, dtype=[('a', 'i4'), ('b', 'i4')])
>>> a == b
array([False, False])
```

Currently, if the dtypes of two void structured arrays are not equivalent the comparison fails,
returning the scalar value `False`. This behavior is deprecated as of numpy 1.10 and will raise an
error or perform elementwise comparison in the future.

The `<` and `>` operators always return `False` when comparing void structured arrays, and
arithmetic and bitwise operations are not supported.

## Record Arrays[¶](https://docs.scipy.org/doc/numpy/user/basics.rec.html#record-arrays "Permalink to this headline")

As an optional convenience numpy provides an ndarray subclass,
[`numpy.recarray`](https://docs.scipy.org/doc/numpy/reference/generated/numpy.recarray.html#numpy.recarray "numpy.recarray"),
and associated helper functions in the `numpy.rec` submodule, that allows access to fields of
structured arrays by attribute instead of only by index. Record arrays also use a special datatype,
[`numpy.record`](https://docs.scipy.org/doc/numpy/reference/generated/numpy.record.html#numpy.record "numpy.record"),
that allows field access by attribute on the structured scalars obtained from the array.

The simplest way to create a record array is with `numpy.rec.array`:

```python
>>> recordarr = np.rec.array([(1,2.,'Hello'),(2,3.,"World")],
...                    dtype=[('foo', 'i4'),('bar', 'f4'), ('baz', 'S10')])
>>> recordarr.bar
array([ 2.,  3.], dtype=float32)
>>> recordarr[1:2]
rec.array([(2, 3.0, 'World')],
      dtype=[('foo', '<i4'), ('bar', '<f4'), ('baz', 'S10')])
>>> recordarr[1:2].foo
array([2], dtype=int32)
>>> recordarr.foo[1:2]
array([2], dtype=int32)
>>> recordarr[1].baz
'World'
```

`numpy.rec.array` can convert a wide variety of arguments into record arrays, including structured
arrays:

```python
>>> arr= array([(1,2.,'Hello'),(2,3.,"World")],
...             dtype=[('foo', 'i4'), ('bar', 'f4'), ('baz', 'S10')])
>>> recordarr = np.rec.array(arr)
```

The `numpy.rec` module provides a number of other convenience functions for creating record arrays,
see
[record array creation routines](https://docs.scipy.org/doc/numpy/reference/routines.array-creation.html#routines-array-creation-rec).

A record array representation of a structured array can be obtained using the appropriate view:

```python
>>> arr = np.array([(1,2.,'Hello'),(2,3.,"World")],
...                dtype=[('foo', 'i4'),('bar', 'f4'), ('baz', 'a10')])
>>> recordarr = arr.view(dtype=dtype((np.record, arr.dtype)),
...                      type=np.recarray)
```

For convenience, viewing an ndarray as type `np.recarray` will automatically convert to `np.record`
datatype, so the dtype can be left out of the view:

```python
>>> recordarr = arr.view(np.recarray)
>>> recordarr.dtype
dtype((numpy.record, [('foo', '<i4'), ('bar', '<f4'), ('baz', 'S10')]))
```

To get back to a plain ndarray both the dtype and type must be reset. The following view does so,
taking into account the unusual case that the recordarr was not a structured type:

```python
>>> arr2 = recordarr.view(recordarr.dtype.fields or recordarr.dtype, np.ndarray)
```

Record array fields accessed by index or by attribute are returned as a record array if the field
has a structured type but as a plain ndarray otherwise.

```python
>>> recordarr = np.rec.array([('Hello', (1,2)),("World", (3,4))],
...                 dtype=[('foo', 'S6'),('bar', [('A', int), ('B', int)])])
>>> type(recordarr.foo)
<type 'numpy.ndarray'>
>>> type(recordarr.bar)
<class 'numpy.core.records.recarray'>
```

Note that if a field has the same name as an ndarray attribute, the ndarray attribute takes
precedence. Such fields will be inaccessible by attribute but will still be accessible by index.

## Recarray Helper Functions[¶](https://docs.scipy.org/doc/numpy/user/basics.rec.html#module-numpy.lib.recfunctions "Permalink to this headline")

Collection of utilities to manipulate structured arrays.

Most of these functions were initially implemented by John Hunter for matplotlib. They have been
rewritten and extended for convenience.

<dl class="function">
    <dt id="numpy.lib.recfunctions.append_fields">
        `numpy.lib.recfunctions.``append_fields`(_base_, _names_, _data_, _dtypes=None_, _fill_value=-1_, _usemask=True_, _asrecarray=False_)[[source]](https://github.com/numpy/numpy/blob/v1.16.1/numpy/lib/recfunctions.py#L678-L746)[¶](https://docs.scipy.org/doc/numpy/user/basics.rec.html#numpy.lib.recfunctions.append_fields "Permalink to this definition")
    </dt>
    <dd>
        Add new fields to an existing array.

        The names of the fields are given with the _names_ arguments,
        the corresponding values with the _data_ arguments.
        If a single field is appended, _names_, _data_ and _dtypes_ do not have
        to be lists but just values.

        <colgroup><col class="field-name">
        <col class="field-body">
        </colgroup>
        Parameters: | <dl class="first last docutils">
            <dt>**base** : array</dt>
            <dd>
                Input array to extend.
            </dd>
            <dt>**names** : string, sequence</dt>
            <dd>
                String or sequence of strings corresponding to the names
                of the new fields.
            </dd>
            <dt>**data** : array or sequence of arrays</dt>
            <dd>
                Array or sequence of arrays storing the fields to add to the base.
            </dd>
            <dt>**dtypes** : sequence of datatypes, optional</dt>
            <dd>
                Datatype or sequence of datatypes.
                If None, the datatypes are estimated from the _data_.
            </dd>
            <dt>**fill_value** : {float}, optional</dt>
            <dd>
                Filling value used to pad missing data on the shorter arrays.
            </dd>
            <dt>**usemask** : {False, True}, optional</dt>
            <dd>
                Whether to return a masked array or not.
            </dd>
            <dt>**asrecarray** : {False, True}, optional</dt>
            <dd>
                Whether to return a recarray (MaskedRecords) or not.
            </dd>
        </dl>
        ----------- | -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

    </dd>

</dl>

<dl class="function">
    <dt id="numpy.lib.recfunctions.drop_fields">
        `numpy.lib.recfunctions.``drop_fields`(_base_, _drop_names_, _usemask=True_, _asrecarray=False_)[[source]](https://github.com/numpy/numpy/blob/v1.16.1/numpy/lib/recfunctions.py#L525-L587)[¶](https://docs.scipy.org/doc/numpy/user/basics.rec.html#numpy.lib.recfunctions.drop_fields "Permalink to this definition")
    </dt>
    <dd>
        Return a new array with fields in _drop_names_ dropped.

        Nested fields are supported.

        <colgroup><col class="field-name">
        <col class="field-body">
        </colgroup>
        Parameters: | <dl class="first last docutils">
            <dt>**base** : array</dt>
            <dd>
                Input array
            </dd>
            <dt>**drop_names** : string or sequence</dt>
            <dd>
                String or sequence of strings corresponding to the names of the
                fields to drop.
            </dd>
            <dt>**usemask** : {False, True}, optional</dt>
            <dd>
                Whether to return a masked array or not.
            </dd>
            <dt>**asrecarray** : string or sequence, optional</dt>
            <dd>
                Whether to return a recarray or a mrecarray (_asrecarray=True_) or
                a plain ndarray or masked array with flexible dtype. The default
                is False.
            </dd>
        </dl>
        ----------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


        Examples

        >>>

        ```python
        >>> from numpy.libimport recfunctions as rfn
        >>> a = np.array([(1, (2, 3.0)), (4, (5, 6.0))],
        ...   dtype=[('a', int), ('b', [('ba', float), ('bb', int)])])
        >>> rfn.drop_fields(a, 'a')
        array([((2.0, 3),), ((5.0, 6),)],
              dtype=[('b', [('ba', '<f8'), ('bb', '<i4')])])
        >>> rfn.drop_fields(a, 'ba')
        array([(1, (3,)), (4, (6,))],
              dtype=[('a', '<i4'), ('b', [('bb', '<i4')])])
        >>> rfn.drop_fields(a, ['ba', 'bb'])
        array([(1,), (4,)],
              dtype=[('a', '<i4')])
        ```
    </dd>

</dl>

<dl class="function">
    <dt id="numpy.lib.recfunctions.find_duplicates">
        `numpy.lib.recfunctions.``find_duplicates`(_a_, _key=None_, _ignoremask=True_, _return_index=False_)[[source]](https://github.com/numpy/numpy/blob/v1.16.1/numpy/lib/recfunctions.py#L1310-L1362)[¶](https://docs.scipy.org/doc/numpy/user/basics.rec.html#numpy.lib.recfunctions.find_duplicates "Permalink to this definition")
    </dt>
    <dd>
        Find the duplicates in a structured array along a given key

        <colgroup><col class="field-name">
        <col class="field-body">
        </colgroup>
        Parameters: | <dl class="first last docutils">
            <dt>**a** : array-like</dt>
            <dd>
                Input array
            </dd>
            <dt>**key** : {string, None}, optional</dt>
            <dd>
                Name of the fields along which to check the duplicates.
                If None, the search is performed by records
            </dd>
            <dt>**ignoremask** : {True, False}, optional</dt>
            <dd>
                Whether masked data should be discarded or considered as duplicates.
            </dd>
            <dt>**return_index** : {False, True}, optional</dt>
            <dd>
                Whether to return the indices of the duplicated values.
            </dd>
        </dl>
        ----------- | -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


        Examples

        >>>

        ```python
        >>> from numpy.libimportrecfunctions as rfn
        >>> ndtype = [('a', int)]
        >>> a = np.ma.array([1, 1, 1, 2, 2, 3, 3],
        ...         mask=[0, 0, 1, 0, 0, 0, 1]).view(ndtype)
        >>> rfn.find_duplicates(a, ignoremask=True, return_index=True)
        ... # XXX: judging by the output, the ignoremask flag has no effect
        ```
    </dd>

</dl>

<dl class="function">
    <dt id="numpy.lib.recfunctions.get_fieldstructure">
        `numpy.lib.recfunctions.``get_fieldstructure`(_adtype_, _lastname=None_, _parents=None_)[[source]](https://github.com/numpy/numpy/blob/v1.16.1/numpy/lib/recfunctions.py#L235-L279)[¶](https://docs.scipy.org/doc/numpy/user/basics.rec.html#numpy.lib.recfunctions.get_fieldstructure "Permalink to this definition")
    </dt>
    <dd>
        Returns a dictionary with fields indexing lists of their parent fields.

        This function is used to simplify access to fields nested in other fields.

        <colgroup><col class="field-name">
        <col class="field-body">
        </colgroup>
        Parameters: | <dl class="first last docutils">
            <dt>**adtype** : np.dtype</dt>
            <dd>
                Input datatype
            </dd>
            <dt>**lastname** : optional</dt>
            <dd>
                Last processed field name (used internally during recursion).
            </dd>
            <dt>**parents** : dictionary</dt>
            <dd>
                Dictionary of parent fields (used interbally during recursion).
            </dd>
        </dl>
        ----------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


        Examples

        >>>

        ```python
        >>> from numpy.libimport recfunctions as rfn
        >>> ndtype =  np.dtype([('A', int),
        ...                     ('B', [('BA', int),
        ...                            ('BB', [('BBA', int), ('BBB', int)])])])
        >>> rfn.get_fieldstructure(ndtype)
        ... # XXX: possible regression, order of BBA and BBB is swapped
        {'A': [], 'B': [], 'BA': ['B'], 'BB': ['B'], 'BBA': ['B', 'BB'], 'BBB': ['B', 'BB']}
        ```
    </dd>

</dl>

<dl class="function">
    <dt id="numpy.lib.recfunctions.join_by">
        `numpy.lib.recfunctions.``join_by`(_key_, _r1_, _r2_, _jointype='inner'_, _r1postfix='1'_, _r2postfix='2'_, _defaults=None_, _usemask=True_, _asrecarray=False_)[[source]](https://github.com/numpy/numpy/blob/v1.16.1/numpy/lib/recfunctions.py#L1371-L1548)[¶](https://docs.scipy.org/doc/numpy/user/basics.rec.html#numpy.lib.recfunctions.join_by "Permalink to this definition")
    </dt>
    <dd>
        Join arrays _r1_ and _r2_ on key _key_.

        The key should be either a string or a sequence of string corresponding
        to the fields used to join the array.  An exception is raised if the
        _key_ field cannot be found in the two input arrays.  Neither _r1_ nor
        _r2_ should have any duplicates along _key_: the presence of duplicates
        will make the output quite unreliable. Note that duplicates are not
        looked for by the algorithm.

        <colgroup><col class="field-name">
        <col class="field-body">
        </colgroup>
        Parameters: | <dl class="first last docutils">
            <dt>**key** : {string, sequence}</dt>
            <dd>
                A string or a sequence of strings corresponding to the fields used
                for comparison.
            </dd>
            <dt>**r1, r2** : arrays</dt>
            <dd>
                Structured arrays.
            </dd>
            <dt>**jointype** : {‘inner’, ‘outer’, ‘leftouter’}, optional</dt>
            <dd>
                If ‘inner’, returns the elements common to both r1 and r2.
                If ‘outer’, returns the common elements as well as the elements of
                r1 not in r2 and the elements of not in r2.
                If ‘leftouter’, returns the common elements and the elements of r1
                not in r2.
            </dd>
            <dt>**r1postfix** : string, optional</dt>
            <dd>
                String appended to the names of the fields of r1 that are present
                in r2 but absent of the key.
            </dd>
            <dt>**r2postfix** : string, optional</dt>
            <dd>
                String appended to the names of the fields of r2 that are present
                in r1 but absent of the key.
            </dd>
            <dt>**defaults** : {dictionary}, optional</dt>
            <dd>
                Dictionary mapping field names to the corresponding default values.
            </dd>
            <dt>**usemask** : {True, False}, optional</dt>
            <dd>
                Whether to return a MaskedArray (or MaskedRecords is
                _asrecarray==True_) or a ndarray.
            </dd>
            <dt>**asrecarray** : {False, True}, optional</dt>
            <dd>
                Whether to return a recarray (or MaskedRecords if _usemask==True_)
                or just a flexible-type ndarray.
            </dd>
        </dl>
        ----------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


        Notes

        * The output is sorted along the key.
        * A temporary array is formed by dropping the fields not in the key for
            the two arrays and concatenating the result. This array is then
            sorted, and the common entries selected. The output is constructed by
            filling the fields with the selected entries. Matching is not
            preserved if there are some duplicates…
    </dd>

</dl>

<dl class="function">
    <dt id="numpy.lib.recfunctions.merge_arrays">
        `numpy.lib.recfunctions.``merge_arrays`(_seqarrays_, _fill_value=-1_, _flatten=False_, _usemask=False_, _asrecarray=False_)[[source]](https://github.com/numpy/numpy/blob/v1.16.1/numpy/lib/recfunctions.py#L383-L518)[¶](https://docs.scipy.org/doc/numpy/user/basics.rec.html#numpy.lib.recfunctions.merge_arrays "Permalink to this definition")
    </dt>
    <dd>
        Merge arrays field by field.

        <colgroup><col class="field-name">
        <col class="field-body">
        </colgroup>
        Parameters: | <dl class="first last docutils">
            <dt>**seqarrays** : sequence of ndarrays</dt>
            <dd>
                Sequence of arrays
            </dd>
            <dt>**fill_value** : {float}, optional</dt>
            <dd>
                Filling value used to pad missing data on the shorter arrays.
            </dd>
            <dt>**flatten** : {False, True}, optional</dt>
            <dd>
                Whether to collapse nested fields.
            </dd>
            <dt>**usemask** : {False, True}, optional</dt>
            <dd>
                Whether to return a masked array or not.
            </dd>
            <dt>**asrecarray** : {False, True}, optional</dt>
            <dd>
                Whether to return a recarray (MaskedRecords) or not.
            </dd>
        </dl>
        ----------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


        Notes

        * Without a mask, the missing value will be filled with something,
            depending on what its corresponding type:
            * `-1`      for integers
            * `-1.0`    for floating point numbers
            * `'-'`     for characters
            * `'-1'`    for strings
            * `True`    for boolean values
        * XXX: I just obtained these values empirically

        Examples

        >>>

        ```python
        >>> from numpy.lib import recfunctions as rfn
        >>> rfn.merge_arrays((np.array([1, 2]), np.array([10., 20., 30.])))
        masked_array(data = [(1, 10.0) (2, 20.0) (--, 30.0)],
                     mask = [(False, False) (False, False) (True, False)],
               fill_value = (999999, 1e+20),
                    dtype = [('f0', '<i4'), ('f1', '<f8')])
        ```

        >>>

        ```python
        >>> rfn.merge_arrays((np.array([1, 2]), np.array([10., 20., 30.])),
        ...              usemask=False)
        array([(1, 10.0), (2, 20.0), (-1, 30.0)],
              dtype=[('f0', '<i4'), ('f1', '<f8')])
        >>> rfn.merge_arrays((np.array([1, 2]).view([('a', int)]),
        ...               np.array([10., 20., 30.])),
        ...              usemask=False, asrecarray=True)
        rec.array([(1, 10.0), (2, 20.0), (-1, 30.0)],
                  dtype=[('a', '<i4'), ('f1', '<f8')])
        ```
    </dd>

</dl>

<dl class="function">
    <dt id="numpy.lib.recfunctions.rec_append_fields">
        `numpy.lib.recfunctions.``rec_append_fields`(_base_, _names_, _data_, _dtypes=None_)[[source]](https://github.com/numpy/numpy/blob/v1.16.1/numpy/lib/recfunctions.py#L755-L787)[¶](https://docs.scipy.org/doc/numpy/user/basics.rec.html#numpy.lib.recfunctions.rec_append_fields "Permalink to this definition")
    </dt>
    <dd>
        Add new fields to an existing array.

        The names of the fields are given with the _names_ arguments,
        the corresponding values with the _data_ arguments.
        If a single field is appended, _names_, _data_ and _dtypes_ do not have
        to be lists but just values.

        <colgroup><col class="field-name">
        <col class="field-body">
        </colgroup>
        Parameters: | <dl class="first docutils">
            <dt>**base** : array</dt>
            <dd>
                Input array to extend.
            </dd>
            <dt>**names** : string, sequence</dt>
            <dd>
                String or sequence of strings corresponding to the names
                of the new fields.
            </dd>
            <dt>**data** : array or sequence of arrays</dt>
            <dd>
                Array or sequence of arrays storing the fields to add to the base.
            </dd>
            <dt>**dtypes** : sequence of datatypes, optional</dt>
            <dd>
                Datatype or sequence of datatypes.
                If None, the datatypes are estimated from the _data_.
            </dd>
        </dl>
        ----------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        Returns:    | <dl class="first last docutils">
            <dt>**appended_array** : np.recarray</dt>
            <dd></dd>
        </dl>

        See also

        [`append_fields`](https://docs.scipy.org/doc/numpy/user/basics.rec.html#numpy.lib.recfunctions.append_fields "numpy.lib.recfunctions.append_fields")
    </dd>

</dl>

<dl class="function">
    <dt id="numpy.lib.recfunctions.rec_drop_fields">
        `numpy.lib.recfunctions.``rec_drop_fields`(_base_, _drop_names_)[[source]](https://github.com/numpy/numpy/blob/v1.16.1/numpy/lib/recfunctions.py#L619-L624)[¶](https://docs.scipy.org/doc/numpy/user/basics.rec.html#numpy.lib.recfunctions.rec_drop_fields "Permalink to this definition")
    </dt>
    <dd>
        Returns a new numpy.recarray with fields in _drop_names_ dropped.
    </dd>
</dl>

<dl class="function">
    <dt id="numpy.lib.recfunctions.rec_join">
        `numpy.lib.recfunctions.``rec_join`(_key_, _r1_, _r2_, _jointype='inner'_, _r1postfix='1'_, _r2postfix='2'_, _defaults=None_)[[source]](https://github.com/numpy/numpy/blob/v1.16.1/numpy/lib/recfunctions.py#L1557-L1570)[¶](https://docs.scipy.org/doc/numpy/user/basics.rec.html#numpy.lib.recfunctions.rec_join "Permalink to this definition")
    </dt>
    <dd>
        Join arrays _r1_ and _r2_ on keys.
        Alternative to join_by, that always returns a np.recarray.

        See also

        <dl class="last docutils">
            <dt>[`join_by`](https://docs.scipy.org/doc/numpy/user/basics.rec.html#numpy.lib.recfunctions.join_by "numpy.lib.recfunctions.join_by")</dt>
            <dd>equivalent function</dd>
        </dl>
    </dd>

</dl>

<dl class="function">
    <dt id="numpy.lib.recfunctions.recursive_fill_fields">
        `numpy.lib.recfunctions.``recursive_fill_fields`(_input_, _output_)[[source]](https://github.com/numpy/numpy/blob/v1.16.1/numpy/lib/recfunctions.py#L40-L77)[¶](https://docs.scipy.org/doc/numpy/user/basics.rec.html#numpy.lib.recfunctions.recursive_fill_fields "Permalink to this definition")
    </dt>
    <dd>
        Fills fields from output with fields from input,
        with support for nested structures.

        <colgroup><col class="field-name">
        <col class="field-body">
        </colgroup>
        Parameters: | <dl class="first last docutils">
            <dt>**input** : ndarray</dt>
            <dd>
                Input array.
            </dd>
            <dt>**output** : ndarray</dt>
            <dd>
                Output array.
            </dd>
        </dl>
        ----------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


        Notes

        * _output_ should be at least the same size as _input_

        Examples

        >>>

        ```python
        >>> from numpy.libimport recfunctions as rfn
        >>> a = np.array([(1, 10.), (2, 20.)], dtype=[('A', int), ('B', float)])
        >>> b = np.zeros((3,), dtype=a.dtype)
        >>> rfn.recursive_fill_fields(a, b)
        array([(1, 10.0), (2, 20.0), (0, 0.0)],
              dtype=[('A', '<i4'), ('B', '<f8')])
        ```
    </dd>

</dl>

<dl class="function">
    <dt id="numpy.lib.recfunctions.rename_fields">
        `numpy.lib.recfunctions.``rename_fields`(_base_, _namemapper_)[[source]](https://github.com/numpy/numpy/blob/v1.16.1/numpy/lib/recfunctions.py#L631-L668)[¶](https://docs.scipy.org/doc/numpy/user/basics.rec.html#numpy.lib.recfunctions.rename_fields "Permalink to this definition")
    </dt>
    <dd>
        Rename the fields from a flexible-datatype ndarray or recarray.

        Nested fields are supported.

        <colgroup><col class="field-name">
        <col class="field-body">
        </colgroup>
        Parameters: | <dl class="first last docutils">
            <dt>**base** : ndarray</dt>
            <dd>
                Input array whose fields must be modified.
            </dd>
            <dt>**namemapper** : dictionary</dt>
            <dd>
                Dictionary mapping old field names to their new version.
            </dd>
        </dl>
        ----------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


        Examples

        >>>

        ```python
        >>> from numpy.libimportrecfunctions as rfn
        >>> a = np.array([(1, (2, [3.0, 30.])), (4, (5, [6.0, 60.]))],
        ...   dtype=[('a', int),('b', [('ba', float), ('bb', (float, 2))])])
        >>> rfn.rename_fields(a, {'a':'A', 'bb':'BB'})
        array([(1, (2.0, [3.0, 30.0])), (4, (5.0, [6.0, 60.0]))],
              dtype=[('A', '<i4'), ('b', [('ba', '<f8'), ('BB', '<f8', 2)])])
        ```
    </dd>

</dl>

<dl class="function">
    <dt id="numpy.lib.recfunctions.stack_arrays">
        `numpy.lib.recfunctions.``stack_arrays`(_arrays_, _defaults=None_, _usemask=True_, _asrecarray=False_, _autoconvert=False_)[[source]](https://github.com/numpy/numpy/blob/v1.16.1/numpy/lib/recfunctions.py#L1218-L1302)[¶](https://docs.scipy.org/doc/numpy/user/basics.rec.html#numpy.lib.recfunctions.stack_arrays "Permalink to this definition")
    </dt>
    <dd>
        Superposes arrays fields by fields

        <colgroup><col class="field-name">
        <col class="field-body">
        </colgroup>
        Parameters: | <dl class="first last docutils">
            <dt>**arrays** : array or sequence</dt>
            <dd>
                Sequence of input arrays.
            </dd>
            <dt>**defaults** : dictionary, optional</dt>
            <dd>
                Dictionary mapping field names to the corresponding default values.
            </dd>
            <dt>**usemask** : {True, False}, optional</dt>
            <dd>
                Whether to return a MaskedArray (or MaskedRecords is
                _asrecarray==True_) or a ndarray.
            </dd>
            <dt>**asrecarray** : {False, True}, optional</dt>
            <dd>
                Whether to return a recarray (or MaskedRecords if _usemask==True_)
                or just a flexible-type ndarray.
            </dd>
            <dt>**autoconvert** : {False, True}, optional</dt>
            <dd>
                Whether automatically cast the type of the field to the maximum.
            </dd>
        </dl>
        ----------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


        Examples

        >>>

        ```python
        >>> from numpy.lib import recfunctions as rfn
        >>> x = np.array([1, 2,])
        >>> rfn.stack_arrays(x) is x
        True
        >>> z = np.array([('A', 1), ('B', 2)], dtype=[('A', '|S3'), ('B', float)])
        >>> zz = np.array([('a', 10., 100.), ('b', 20., 200.), ('c', 30., 300.)],
        ...   dtype=[('A', '|S3'), ('B', float), ('C', float)])
        >>> test = rfn.stack_arrays((z,zz))
        >>> test
        masked_array(data = [('A', 1.0, --) ('B', 2.0, --) ('a', 10.0, 100.0) ('b', 20.0, 200.0)
         ('c', 30.0, 300.0)],
                     mask = [(False, False, True) (False, False, True) (False, False, False)
         (False, False, False) (False, False, False)],
               fill_value = ('N/A', 1e+20, 1e+20),
                    dtype = [('A', '|S3'), ('B', '<f8'), ('C', '<f8')])
        ```
    </dd>

</dl>
