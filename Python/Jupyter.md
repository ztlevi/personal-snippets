# Jupyter

## Jupyter executable

```
import sys
sys.executable
```

## Plot

This is a bit of magic to make matplotlib figures appear inline in the notebook rather than in a new
window.

```python
import matplotlib.pyplot as plt

%matplotlib inline
plt.rcParams['figure.figsize'] = (10.0, 8.0) # set default size of plots
plt.rcParams['image.interpolation'] = 'nearest'
plt.rcParams['image.cmap'] = 'gray'
```

## Show Image

```python
from IPython.display import Image # for ipython
import base64
with open('test.jpg', 'wb') as f:
    f.write(image_data)
Image(filename="test.jpg", width=224)
```

## Print function

That's the whole point of `from __future__ import print_function;` to bring the print function from
Python 3 into Python 2.6+

```python
from __future__ import print_function
```

## Reload module

Some more magic so that the notebook will reload external python modules;

see http://stackoverflow.com/questions/1907993/autoreload-of-modules-in-ipython

```python
%load_ext autoreload
%autoreload 2
```

## time

```python
%%time
a = [i for i in range(100000)]
b = [i for i in range(100000)]
```

Wall time: 23.4 ms CPU times: user 13.6 ms, sys: 10.4 ms, total: 24 ms

## Add toggle code button

```
from IPython.display import HTML
```
