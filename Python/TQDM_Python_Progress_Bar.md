# TQDM

### [Usage](https://github.com/tqdm/tqdm#table-of-contents)

`tqdm` is very versatile and can be used in a number of ways. The three main ones are given below.

#### [Iterable-based](https://github.com/tqdm/tqdm#table-of-contents)

Wrap `tqdm()` around any iterable:

```python
from tqdm import tqdm
import time

text = ""
for char in tqdm(["a", "b", "c", "d"]):
    time.sleep(0.25)
    text = text + char
```

`trange(i)` is a special optimised instance of `tqdm(range(i))`:

```python
for i in trange(100):
    time.sleep(0.01)
```

Instantiation outside of the loop allows for manual control over `tqdm()`:

```python
pbar = tqdm(["a", "b", "c", "d"])
for char in pbar:
    time.sleep(0.25)
    pbar.set_description("Processing %s" % char)
```

#### [Manual](https://github.com/tqdm/tqdm#table-of-contents)

Manual control on `tqdm()` updates by using a `with` statement:

```python
with tqdm(total=100) as pbar:
    for i in range(10):
        time.sleep(0.1)
        pbar.update(10)
```

If the optional variable `total` (or an iterable with `len()`) is provided, predictive stats are displayed.

`with` is also optional (you can just assign `tqdm()` to a variable, but in this case don't forget to `del` or `close()`
at the end:

```python
pbar = tqdm(total=100)
for i in range(10):
    time.sleep(0.1)
    pbar.update(10)
pbar.close()
```

#### [Module](https://github.com/tqdm/tqdm#table-of-contents)

Perhaps the most wonderful use of `tqdm` is in a script or on the command line. Simply inserting `tqdm` (or
`python -m tqdm`) between pipes will pass through all `stdin` to `stdout` while printing progress to `stderr`.

The example below demonstrated counting the number of lines in all Python files in the current directory, with timing
information included.

```python
$ time find . -name '*.py' -type f -exec cat \{} \; | wc -l
857365

real    0m3.458s
user    0m0.274s
sys     0m3.325s

$ time find . -name '*.py' -type f -exec cat \{} \; | tqdm | wc -l
857366it [00:03, 246471.31it/s]
857365

real    0m3.585s
user    0m0.862s
sys     0m3.358s
```

Note that the usual arguments for `tqdm` can also be specified.

```python
$ find . -name '*.py' -type f -exec cat \{} \; |
    tqdm --unit loc --unit_scale --total 857366 >> /dev/null
100%|███████████████████████████████████| 857K/857K [00:04<00:00, 246Kloc/s]
```

Backing up a large directory?

```python
$ 7z a -bd -r backup.7z docs/ | grep Compressing |
    tqdm --total $(find docs/ -type f | wc -l) --unit files >> backup.log
100%|███████████████████████████████▉| 8014/8014 [01:37<00:00, 82.29files/s]
```

#### Multiprocessing with TQDM

```python
import random
import time
from multiprocessing import Pool, Lock

from tqdm import tqdm


def myfunc(a):
    time.sleep(random.random())
    return a**2


pool = Pool(2)
'''
for _ in tqdm(pool.imap_unordered(myfunc, range(100)), total=100):
    pass
'''
with tqdm(total=100) as pbar:
    lock = Lock()
    def update(*args, **kwargs):
        with lock:
            pbar.update()
            # tqdm.write(str(a))


    for i in range(pbar.total):
        pool.apply_async(myfunc, args=(i, ), kwds={}, callback=update)
    # tqdm.write('scheduled')
    pool.close()
    pool.join()
```
