## [Cache](http://www.grantjenks.com/docs/diskcache/tutorial.html#id2)[¶](http://www.grantjenks.com/docs/diskcache/tutorial.html#cache "Permalink to this headline")

The core of [DiskCache](http://www.grantjenks.com/docs/diskcache/index.html) is
[`diskcache.Cache`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache "diskcache.Cache")
which represents a disk and file backed cache. As a Cache, it supports a familiar Python mapping
interface with additional cache and performance parameters.

```python
>>> from diskcache import Cache
>>> cache = Cache()
```

Initialization expects a directory path reference. If the directory path does not exist, it will be
created. When not specified, a temporary directory is automatically created. Additional keyword
parameters are discussed below. Cache objects are thread-safe and may be shared between threads. Two
Cache objects may also reference the same directory from separate threads or processes. In this way,
they are also process-safe and support cross-process communication.

Cache objects open and maintain one or more file handles. But unlike files, all Cache operations are
atomic and Cache objects support process-forking and may be serialized using Pickle. Each thread
that accesses a cache should also call
[`close`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.close "diskcache.Cache.close")
on the cache. Cache objects can be used in a <cite>with</cite> statement to safeguard calling
[`close`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.close "diskcache.Cache.close").

```python
>>> cache.close()
>>> with Cache(cache.directory) as reference:
...     reference.set('key', 'value')
True
```

Closed Cache objects will automatically re-open when accessed. But opening Cache objects is
relatively slow, and since all operations are atomic, may be safely left open.

```python
>>> cache.close()
>>> cache.get('key')  # Automatically opens, but slower.
'value'
```

Set an item, get a value, and delete a key using the usual operators:

```python
>>> cache['key'] = 'value'
>>> cache['key']
'value'
>>> 'key' in cache
True
>>> del cache['key']
```

There’s also a
[`set`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.set "diskcache.Cache.set")
method with additional keyword parameters: <cite>expire</cite>, <cite>read</cite>, and
<cite>tag</cite>.

```python
>>> from io import BytesIO
>>> cache.set('key', BytesIO(b'value'), expire=5, read=True, tag='data')
True
```

In the example above: the key expires in 5 seconds, the value is read as a file-like object, and tag
metadata is stored with the key. Another method,
[`get`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.get "diskcache.Cache.get")
supports querying extra information with <cite>default</cite>, <cite>read</cite>,
<cite>expire_time</cite>, and <cite>tag</cite> keyword parameters.

```python
>>> result=cache.get('key', read=True, expire_time=True, tag=True)
>>> reader, timestamp, tag = result
>>> print(reader.read().decode())
value
>>> type(timestamp).__name__
'float'
>>> print(tag)
data
```

The return value is a tuple containing the value, expire time (seconds from epoch), and tag. Because
we passed `read=True` the value is returned as a file-like object.

Use
[`touch`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.touch "diskcache.Cache.touch")
to update the expiration time of an item in the cache.

```python
>>> cache.touch('key', expire=None)
True
>>> cache.touch('does-not-exist', expire=1)
False
```

Like
[`set`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.set "diskcache.Cache.set"),
the method
[`add`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.add "diskcache.Cache.add")
can be used to insert an item in the cache. The item is inserted only if the key is not already
present.

```python
>>> cache.add(b'test', 123)
True
>>> cache[b'test']
123
>>> cache.add(b'test', 456)
False
>>> cache[b'test']
123
```

Item values can also be incremented and decremented using
[`incr`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.incr "diskcache.Cache.incr")
and
[`decr`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.decr "diskcache.Cache.decr")
methods.

```python
>>> cache.incr(b'test')
124
>>> cache.decr(b'test', 24)
100
```

Increment and decrement methods also support a keyword parameter, <cite>default</cite>, which will
be used for missing keys. When `None`, incrementing or decrementing a missing key will raise a
`KeyError`.

```python
>>> cache.incr('alice')
1
>>> cache.decr('bob', default=-9)
-10
>>> cache.incr('carol', default=None)
Traceback (most recent call last):
    ...
KeyError: 'carol'
```

Increment and decrement operations are atomic and assume the value may be stored in a SQLite integer
column. SQLite supports 64-bit signed integers.

Like
[`delete`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.delete "diskcache.Cache.delete")
and
[`get`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.get "diskcache.Cache.get"),
the method
[`pop`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.pop "diskcache.Cache.pop")
can be used to delete an item in the cache and return its value.

```python
>>> cache.pop('alice')1
>>> cache.pop('dave', default='does not exist')
'does not exist'
>>> cache.set('dave', 0, expire=None, tag='admin')
True
>>> result = cache.pop('dave', expire_time=True, tag=True)
>>> value, timestamp, tag = result
>>> value
0
>>> print(timestamp)
None
>>> print(tag)
admin
```

The
[`pop`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.pop "diskcache.Cache.pop")
operation is atomic and using
[`incr`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.incr "diskcache.Cache.incr")
together is an accurate method for counting and dumping statistics in long-running systems. Unlike
[`get`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.get "diskcache.Cache.get")
the <cite>read</cite> argument is not supported.

Another four methods remove items from the cache:

```python
>>> cache.clear()
3
>>> cache.reset('cull_limit', 0)       # Disable automatic evictions.
0
>>> for num in range(10):
...     _ = cache.set(num, num, expire=1e-9)  # Expire immediately.
>>> len(cache)
10
>>> list(cache)
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
>>> import time
>>> time.sleep(1)
>>> cache.expire()
10
```

[`Expire`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.expire "diskcache.Cache.expire")
removes all expired keys from the cache. Resetting the
[cull_limit](http://www.grantjenks.com/docs/diskcache/tutorial.html#tutorial-settings) to zero will
disable culling during
[`set`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.set "diskcache.Cache.set")
and
[`add`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.add "diskcache.Cache.add")
operations. Because culling is performed lazily, the reported length of the cache includes expired
items. Iteration likewise includes expired items because it is a read-only operation. To exclude
expired items you must explicitly call
[`expire`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.expire "diskcache.Cache.expire")
which works regardless of the
[cull_limit](http://www.grantjenks.com/docs/diskcache/tutorial.html#tutorial-settings).

```python
>>> for num in range(100):
...     _ = cache.set(num, num, tag='odd' if num % 2 else 'even')
>>> cache.evict('even')
50
```

[`Evict`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.evict "diskcache.Cache.evict")
removes all the keys with a matching tag. The default tag is `None`. Tag values may be any of
integer, float, string, bytes and None. To accelerate the eviction of items by tag, an index can be
created. To do so, initialize the cache with `tag_index=True`.

```python
>>> cache.clear()
50
>>> for num in range(100):
...     _ = cache.set(num, num, tag=(num % 2))
>>> cache.evict(0)
50
```

Likewise, the tag index may be created or dropped using methods:

```python
>>> cache.drop_tag_index()
>>> cache.tag_index
0
>>> cache.create_tag_index()
>>> cache.tag_index
1
```

But prefer initializing the cache with a tag index rather than explicitly creating or dropping the
tag index.

To manually enforce the cache’s size limit, use the
[`cull`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.cull "diskcache.Cache.cull")
method.
[`Cull`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.cull "diskcache.Cache.cull")
begins by removing expired items from the cache and then uses the eviction policy to remove items
until the cache volume is less than the size limit.

```python
>>> cache.clear()
50
>>> cache.reset('size_limit', int(1e6))
1000000
>>> cache.reset('cull_limit', 0)
0
>>> for count in range(1000):
...     cache[count] = b'A' * 1000
>>> cache.volume() > int(1e6)
True
>>> cache.cull() > 0
True
>>> cache.volume() < int(1e6)
True
```

Some users may defer all culling to a cron-like process by setting the
[cull_limit](http://www.grantjenks.com/docs/diskcache/tutorial.html#tutorial-settings) to zero and
manually calling
[`cull`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.cull "diskcache.Cache.cull")
to remove items. Like
[`evict`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.evict "diskcache.Cache.evict")
and
[`expire`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.expire "diskcache.Cache.expire"),
calls to `cull` will work regardless of the
[cull_limit](http://www.grantjenks.com/docs/diskcache/tutorial.html#tutorial-settings).

[`Clear`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.clear "diskcache.Cache.clear")
simply removes all items from the cache.

```python
>>> cache.clear() > 0
True
```

Each of these methods is designed to work concurrent to others. None of them block readers or
writers in other threads or processes.

Caches may be iterated by either insertion order or sorted order. The default ordering uses
insertion order. To iterate by sorted order, use
[`iterkeys`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.iterkeys "diskcache.Cache.iterkeys").
The sort order is determined by the database which makes it valid only for <cite>str</cite>,
<cite>bytes</cite>, <cite>int</cite>, and <cite>float</cite> data types. Other types of keys will be
serialized which is likely to have a meaningless sorted order.

```python
>>> for key in 'cab':
...     cache[key] = None
>>> list(cache)
['c', 'a', 'b']
>>> list(cache.iterkeys())
['a', 'b', 'c']
>>> cache.peekitem()
('b', None)
>>> cache.peekitem(last=False)
('c', None)
```

If only the first or last item in insertion order is desired then
[`peekitem`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.peekitem "diskcache.Cache.peekitem")
is more efficient than using iteration.

Three additional methods use the sorted ordering of keys to maintain a queue-like data structure
within the cache. The
[`push`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.push "diskcache.Cache.push"),
[`pull`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.pull "diskcache.Cache.pull"),
and
[`peek`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.peek "diskcache.Cache.peek")
methods automatically assign the key within the cache.

```python
>>> key= cache.push('first')
>>> print(key)
500000000000000
>>> cache[key]
'first'
>>> _ = cache.push('second')
>>> _ = cache.push('zeroth', side='front')
>>> _, value = cache.peek()
>>> value
'zeroth'
>>> key, value = cache.pull()
>>> print(key)
499999999999999
>>> value
'zeroth'
```

The <cite>side</cite> parameter supports access to either the `'front'` or `'back'` of the cache. In
addition, the <cite>prefix</cite> parameter can be used to maintain multiple queue-like data
structures within a single cache. When prefix is `None`, integer keys are used. Otherwise, string
keys are used in the format “prefix-integer”. Integer starts at 500 trillion. Like
[`set`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.set "diskcache.Cache.set")
and
[`get`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.get "diskcache.Cache.get"),
methods
[`push`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.push "diskcache.Cache.push"),
[`pull`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.pull "diskcache.Cache.pull"),
and
[`peek`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.peek "diskcache.Cache.peek")
support cache metadata like the expiration time and tag.

Lastly, three methods support metadata about the cache. The first is
[`volume`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.volume "diskcache.Cache.volume")
which returns the estimated total size in bytes of the cache directory on disk.

```python
>>> cache.volume() < int(1e5)
True
```

The second is
[`stats`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.stats "diskcache.Cache.stats")
which returns cache hits and misses. Cache statistics must first be enabled.

```python
>>> cache.stats(enable=True)
(0, 0)
>>> for num in range(100):
...     _ = cache.set(num, num)
>>> for num in range(150):
...     _ = cache.get(num)
>>> hits, misses = cache.stats(enable=False, reset=True)
>>> (hits, misses)
(100, 50)
```

Cache statistics are useful when evaluating different
[eviction policies](http://www.grantjenks.com/docs/diskcache/tutorial.html#tutorial-eviction-policies).
By default, statistics are disabled as they incur an extra overhead on cache lookups. Increment and
decrement operations are not counted in cache statistics.

The third is
[`check`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.check "diskcache.Cache.check")
which verifies cache consistency. It can also fix inconsistencies and reclaim unused space. The
return value is a list of warnings.

```python
>>> warnings = cache.check()
```

Caches do not automatically remove the underlying directory where keys and values are stored. The
cache is intended to be persistent and so must be deleted manually.

```python
>>> cache.close()
>>> import shutil
>>> try:
...     shutil.rmtree(cache.directory)
... except OSError:  # Windows wonkiness
...     pass
```

To permanently delete the cache, recursively remove the cache’s directory.
