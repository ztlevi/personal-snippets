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

## [FanoutCache](http://www.grantjenks.com/docs/diskcache/tutorial.html#id3)[¶](http://www.grantjenks.com/docs/diskcache/tutorial.html#fanoutcache "Permalink to this headline")

Built atop
[`Cache`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache "diskcache.Cache") is
[`diskcache.FanoutCache`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.FanoutCache "diskcache.FanoutCache")
which automatically <cite>shards</cite> the underlying database.
[Sharding](<https://en.wikipedia.org/wiki/Shard_(database_architecture)>) is the practice of
horizontally partitioning data. Here it is used to decrease blocking writes. While readers and
writers do not block each other, writers block other writers. Therefore a shard for every concurrent
writer is suggested. This will depend on your scenario. The default value is 8.

Another parameter, <cite>timeout</cite>, sets a limit on how long to wait for database transactions.
Transactions are used for every operation that writes to the database. When the timeout expires, a
[`diskcache.Timeout`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Timeout "diskcache.Timeout")
error is raised internally. This <cite>timeout</cite> parameter is also present on
[`diskcache.Cache`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache "diskcache.Cache").
When a
[`Timeout`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Timeout "diskcache.Timeout")
error occurs in
[`Cache`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache "diskcache.Cache")
methods, the exception may be raised to the caller. In contrast,
[`FanoutCache`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.FanoutCache "diskcache.FanoutCache")
catches all timeout errors and aborts the operation. As a result,
[`set`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.FanoutCache.set "diskcache.FanoutCache.set")
and
[`delete`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.FanoutCache.delete "diskcache.FanoutCache.delete")
methods may silently fail.

Most methods that handle
[`Timeout`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Timeout "diskcache.Timeout")
exceptions also include a <cite>retry</cite> keyword parameter (default `False`) to automatically
repeat attempts that timeout. The mapping interface operators:
[`cache[key]`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.FanoutCache.__getitem__ "diskcache.FanoutCache.__getitem__"),
[`cache[key] = value`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.FanoutCache.__setitem__ "diskcache.FanoutCache.__setitem__"),
and
[`del cache[key]`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.FanoutCache.__delitem__ "diskcache.FanoutCache.__delitem__")
automatically retry operations when
[`Timeout`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Timeout "diskcache.Timeout")
errors occur.
[`FanoutCache`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.FanoutCache "diskcache.FanoutCache")
will never raise a
[`Timeout`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Timeout "diskcache.Timeout")
exception. The default <cite>timeout</cite> is 0.010 (10 milliseconds).

```python
>>> from diskcache import FanoutCache
>>> cache = FanoutCache(shards=4, timeout=1)
```

The example above creates a cache in a temporary directory with four shards and a one second
timeout. Operations will attempt to abort if they take longer than one second. The remaining API of
[`FanoutCache`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.FanoutCache "diskcache.FanoutCache")
matches
[`Cache`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache "diskcache.Cache") as
described above.

The
[`FanoutCache`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.FanoutCache "diskcache.FanoutCache")
[size_limit](http://www.grantjenks.com/docs/diskcache/api.html#constants) is used as the total size
of the cache. The size limit of individual cache shards is the total size divided by the number of
shards. In the example above, the default total size is one gigabyte and there are four shards so
each cache shard has a size limit of 256 megabytes. Items that are larger than the size limit are
immediately culled.

Caches have an additional feature:
[`memoizing`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.FanoutCache.memoize "diskcache.FanoutCache.memoize")
decorator. The decorator wraps a callable and caches arguments and return values.

```python
>>> from diskcacheimport FanoutCache
>>> cache = FanoutCache()
>>> @cache.memoize(typed=True, expire=1, tag='fib')
... def fibonacci(number):
...     if number == 0:
...         return 0
...     elif number == 1:
...         return 1
...     else:
...         return fibonacci(number - 1) + fibonacci(number - 2)
>>> print(sum(fibonacci(value) for value in range(100)))
573147844013817084100
```

The arguments to memoize are like those for
[functools.lru_cache](https://docs.python.org/3/library/functools.html#functools.lru_cache) and
[`Cache.set`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.set "diskcache.Cache.set").
Remember to call
[`memoize`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.FanoutCache.memoize "diskcache.FanoutCache.memoize")
when decorating a callable. If you forget, then a TypeError will occur:

```python
>>> @cache.memoize
... def test():
...     pass
Traceback (most recent call last):
    ...
TypeError: name cannot be callable
```

Observe the lack of parenthenses after
[`memoize`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.FanoutCache.memoize "diskcache.FanoutCache.memoize")
above.

## [DjangoCache](http://www.grantjenks.com/docs/diskcache/tutorial.html#id4)[¶](http://www.grantjenks.com/docs/diskcache/tutorial.html#djangocache "Permalink to this headline")

[`diskcache.DjangoCache`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.DjangoCache "diskcache.DjangoCache")
uses
[`FanoutCache`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.FanoutCache "diskcache.FanoutCache")
to provide a Django-compatible cache interface. With
[DiskCache](http://www.grantjenks.com/docs/diskcache/index.html) installed, you can use
[`DjangoCache`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.DjangoCache "diskcache.DjangoCache")
in your settings file.

```python
CACHES = {'default': {
        'BACKEND': 'diskcache.DjangoCache',
        'LOCATION': '/path/to/cache/directory',
        'TIMEOUT': 300,
        # ^-- Django setting for default timeout of each key.
        'SHARDS': 8,
        'DATABASE_TIMEOUT': 0.010,  # 10 milliseconds
        # ^-- Timeout for each DjangoCache database transaction.
        'OPTIONS': {
            'size_limit': 2 ** 30   # 1 gigabyte
        },
    },
}
```

As with
[`FanoutCache`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.FanoutCache "diskcache.FanoutCache")
above, these settings create a Django-compatible cache with eight shards and a 10ms timeout. You can
pass further settings via the `OPTIONS` mapping as shown in the Django documentation. Only the
`BACKEND` and `LOCATION` keys are necessary in the above example. The other keys simply display
their default value.
[`DjangoCache`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.DjangoCache "diskcache.DjangoCache")
will never raise a
[`Timeout`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Timeout "diskcache.Timeout")
exception. But unlike
[`FanoutCache`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.FanoutCache "diskcache.FanoutCache"),
the keyword parameter <cite>retry</cite> defaults to `True` for
[`DjangoCache`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.DjangoCache "diskcache.DjangoCache")
methods.

The API of
[`DjangoCache`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.DjangoCache "diskcache.DjangoCache")
is a superset of the functionality described in the
[Django documentation on caching](https://docs.djangoproject.com/en/1.9/topics/cache/#the-low-level-cache-api)
and includes many
[`FanoutCache`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.FanoutCache "diskcache.FanoutCache")
features.

[`DjangoCache`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.DjangoCache "diskcache.DjangoCache")
also works well with <cite>X-Sendfile</cite> and <cite>X-Accel-Redirect</cite> headers.

```python
fromdjango.core.cache import cache

def media(request, path):
    try:
        with cache.read(path) as reader:
            response = HttpResponse()
            response['X-Accel-Redirect'] = reader.name
            return response
    except KeyError:
        # Handle cache miss.
```

When values are
[`set`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.DjangoCache.set "diskcache.DjangoCache.set")
using `read=True` they are guaranteed to be stored in files. The full path is available on the file
handle in the <cite>name</cite> attribute. Remember to also include the <cite>Content-Type</cite>
header if known.

## [Deque](http://www.grantjenks.com/docs/diskcache/tutorial.html#id5)[¶](http://www.grantjenks.com/docs/diskcache/tutorial.html#deque "Permalink to this headline")

[`diskcache.Deque`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Deque "diskcache.Deque")
(pronounced “deck”) uses a
[`Cache`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache "diskcache.Cache") to
provide a
[collections.deque](https://docs.python.org/3/library/collections.html#collections.deque)-compatible
double-ended queue. Deques are a generalization of stacks and queues with fast access and editing at
both front and back sides.
[`Deque`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Deque "diskcache.Deque")
objects use the
[`push`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.push "diskcache.Cache.push"),
[`pull`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.pull "diskcache.Cache.pull"),
and
[`peek`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.peek "diskcache.Cache.peek")
methods of
[`Cache`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache "diskcache.Cache")
objects but never evict or expire items.

```python
>>> from diskcache importDeque
>>> deque = Deque(range(5, 10))
>>> deque.pop()
9
>>> deque.popleft()
5
>>> deque.appendleft('foo')
>>> len(deque)
4
>>> type(deque.directory).__name__
'str'
>>> other = Deque(directory=deque.directory)
>>> len(other)
4
>>> other.popleft()
'foo'
```

[`Deque`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Deque "diskcache.Deque")
objects provide an efficient and safe means of cross-thread and cross-process communication.
[`Deque`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Deque "diskcache.Deque")
objects are also useful in scenarios where contents should remain persistent or limitations prohibit
holding all items in memory at the same time. The deque uses a fixed amout of memory regardless of
the size or number of items stored inside it.

## [Index](http://www.grantjenks.com/docs/diskcache/tutorial.html#id6)[¶](http://www.grantjenks.com/docs/diskcache/tutorial.html#index "Permalink to this headline")

[`diskcache.Index`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Index "diskcache.Index")
uses a
[`Cache`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache "diskcache.Cache") to
provide a
[mutable mapping](https://docs.python.org/3/library/collections.abc.html#collections-abstract-base-classes)
and [ordered dictionary](https://docs.python.org/3/library/collections.html#collections.OrderedDict)
interface.
[`Index`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Index "diskcache.Index")
objects inherit all the benefits of
[`Cache`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache "diskcache.Cache")
objects but never evict or expire items.

```python
>>> from diskcacheimport Index
>>> index = Index([('a', 1), ('b', 2), ('c', 3)])
>>> 'b' in index
True
>>> index['c']
3
>>> del index['a']
>>> len(index)
2
>>> other = Index(index.directory)
>>> len(other)
2
>>> other.popitem(last=False)
('b', 2)
```

[`Index`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Index "diskcache.Index")
objects provide an efficient and safe means of cross-thread and cross-process communication.
[`Index`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Index "diskcache.Index")
objects are also useful in scenarios where contents should remain persistent or limitations prohibit
holding all items in memory at the same time. The index uses a fixed amout of memory regardless of
the size or number of items stored inside it.

## [Transactions](http://www.grantjenks.com/docs/diskcache/tutorial.html#id7)[¶](http://www.grantjenks.com/docs/diskcache/tutorial.html#transactions "Permalink to this headline")

Transactions are implemented by the
[`Cache`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache "diskcache.Cache"),
[`Deque`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Deque "diskcache.Deque"), and
[`Index`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Index "diskcache.Index") data
types and support consistency and improved performance. Use transactions to guarantee a group of
operations occur atomically. For example, to calculate a running average, the total and count could
be incremented together:

```python
>>> with cache.transact():
...     total = cache.incr('total', 123.45)
...     count = cache.incr('count')
>>> total
123.45
>>> count
1
```

And to calculate the average, the values could be retrieved together:

```python
>>> with cache.transact():
...     total = cache.get('total')
...     count = cache.get('count')
>>> average = None if count == 0 else total / count
>>> average
123.45
```

Keep transactions as short as possible because within a transaction, no other writes may occur to
the cache. Every write operation uses a transaction and transactions may be nested to improve
performance. For example, a possible implementation to set many items within the cache:

```python
>>> defset_many(cache, mapping):
...     with cache.transact():
...         for key, value in mapping.items():
...             cache[key] = value
```

By grouping all operations in a single transaction, performance may improve two to five times. But
be careful, a large mapping will block other concurrent writers.

Transactions are not implemented by
[`FanoutCache`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.FanoutCache "diskcache.FanoutCache")
and
[`DjangoCache`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.DjangoCache "diskcache.DjangoCache")
due to key sharding. Instead, a cache shard with transaction support may be requested.

```python
>>> fanout_cache = FanoutCache()
>>> tutorial_cache = fanout_cache.cache('tutorial')
>>> username_queue = fanout_cache.deque('usernames')
>>> url_to_response = fanout_cache.index('responses')
```

The cache shard exists in a subdirectory of the fanout-cache with the given name.

## [Recipes](http://www.grantjenks.com/docs/diskcache/tutorial.html#id8)[¶](http://www.grantjenks.com/docs/diskcache/tutorial.html#recipes "Permalink to this headline")

[DiskCache](http://www.grantjenks.com/docs/diskcache/index.html) includes a few synchronization
recipes for cross-thread and cross-process communication:

- [`Averager`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Averager "diskcache.Averager")
  – maintains a running average like that shown above.

- [`Lock`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Lock "diskcache.Lock"),
  [`RLock`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.RLock "diskcache.RLock"),
  and
  [`BoundedSemaphore`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.BoundedSemaphore "diskcache.BoundedSemaphore")
  – recipes for synchronization around critical sections like those found in Python’s
  [threading](https://docs.python.org/3/library/threading.html) and
  [multiprocessing](https://docs.python.org/3/library/multiprocessing.html) modules.

- [`throttle`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.throttle "diskcache.throttle")
  – function decorator to rate-limit calls to a function.

- [`barrier`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.barrier "diskcache.barrier")
  – function decorator to synchronize calls to a function.

- [`memoize_stampede`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.memoize_stampede "diskcache.memoize_stampede")
  – memoizing function decorator with cache stampede protection. Read
  [Case Study: Landing Page Caching](http://www.grantjenks.com/docs/diskcache/case-study-landing-page-caching.html)
  for a comparison of memoization strategies.

## [Settings](http://www.grantjenks.com/docs/diskcache/tutorial.html#id9)[¶](http://www.grantjenks.com/docs/diskcache/tutorial.html#settings "Permalink to this headline")

A variety of settings are available to improve performance. These values are stored in the database
for durability and to communicate between processes. Each value is cached in an attribute with
matching name. Attributes are updated using
[`reset`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.reset "diskcache.Cache.reset").
Attributes are set during initialization when passed as keyword arguments.

- <cite>size_limit</cite>, default one gigabyte. The maximum on-disk size of the cache.

- <cite>cull_limit</cite>, default ten. The maximum number of keys to cull when adding a new item.
  Set to zero to disable automatic culling. Some systems may disable automatic culling in exchange
  for a cron-like job that regularly calls
  [`cull`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.cull "diskcache.Cache.cull")
  in a separate process.

- <cite>statistics</cite>, default False, disabled. The setting to collect
  [cache statistics](http://www.grantjenks.com/docs/diskcache/tutorial.html#tutorial-statistics).

- <cite>tag_index</cite>, default False, disabled. The setting to create a database
  [tag index](http://www.grantjenks.com/docs/diskcache/tutorial.html#tutorial-tag-index) for
  [`evict`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.evict "diskcache.Cache.evict").

- <cite>eviction_policy</cite>, default “least-recently-stored”. The setting to determine
  [eviction policy](http://www.grantjenks.com/docs/diskcache/tutorial.html#tutorial-eviction-policies).

The
[`reset`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.FanoutCache.reset "diskcache.FanoutCache.reset")
method accepts an optional second argument that updates the corresponding value in the database. The
return value is the latest retrieved from the database. Notice that attributes are updated lazily.
Prefer idioms like
[`len`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.FanoutCache.__len__ "diskcache.FanoutCache.__len__"),
[`volume`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.FanoutCache.volume "diskcache.FanoutCache.volume"),
and
[`keyword arguments`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.FanoutCache.__init__ "diskcache.FanoutCache.__init__")
rather than using
[`reset`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.FanoutCache.reset "diskcache.FanoutCache.reset")
directly.

```python
>>> cache= Cache(size_limit=int(4e9))
>>> print(cache.size_limit)
4000000000
>>> cache.disk_min_file_size
32768
>>> cache.reset('cull_limit', 0)  # Disable automatic evictions.
0
>>> cache.set(b'key', 1.234)
True
>>> cache.count           # Stale attribute.
0
>>> cache.reset('count')  # Prefer: len(cache)
1
```

More settings correspond to
[Disk](http://www.grantjenks.com/docs/diskcache/tutorial.html#tutorial-disk) attributes. Each of
these may be specified when initializing the
[Cache](http://www.grantjenks.com/docs/diskcache/tutorial.html#tutorial-cache). Changing these
values will update the unprefixed attribute on the
[`Disk`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Disk "diskcache.Disk") object.

- <cite>disk_min_file_size</cite>, default 32 kilobytes. The minimum size to store a value in a
  file.

- <cite>disk_pickle_protocol</cite>, default highest Pickle protocol. The Pickle protocol to use for
  data types that are not natively supported.

An additional set of attributes correspond to SQLite pragmas. Changing these values will also
execute the appropriate `PRAGMA` statement. See the
[SQLite pragma documentation](https://www.sqlite.org/pragma.html) for more details.

- <cite>sqlite_auto_vacuum</cite>, default 1, “FULL”.

- <cite>sqlite_cache_size</cite>, default 8,192 pages.

- <cite>sqlite_journal_mode</cite>, default “wal”.

- <cite>sqlite_mmap_size</cite>, default 64 megabytes.

- <cite>sqlite_synchronous</cite>, default 1, “NORMAL”.

Each of these settings can passed to
[`DjangoCache`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.DjangoCache "diskcache.DjangoCache")
via the `OPTIONS` key mapping. Always measure before and after changing the default values. Default
settings are programmatically accessible at `diskcache.DEFAULT_SETTINGS`.

## [Eviction Policies](http://www.grantjenks.com/docs/diskcache/tutorial.html#id10)[¶](http://www.grantjenks.com/docs/diskcache/tutorial.html#eviction-policies "Permalink to this headline")

[DiskCache](http://www.grantjenks.com/docs/diskcache/index.html) supports four eviction policies
each with different tradeoffs for accessing and storing items.

- `"least-recently-stored"` is the default. Every cache item records the time it was stored in the
  cache. This policy adds an index to that field. On access, no update is required. Keys are evicted
  starting with the oldest stored keys. As
  [DiskCache](http://www.grantjenks.com/docs/diskcache/index.html) was intended for large caches
  (gigabytes) this policy usually works well enough in practice.

- `"least-recently-used"` is the most commonly used policy. An index is added to the access time
  field stored in the cache database. On every access, the field is updated. This makes every access
  into a read and write which slows accesses.

- `"least-frequently-used"` works well in some cases. An index is added to the access count field
  stored in the cache database. On every access, the field is incremented. Every access therefore
  requires writing the database which slows accesses.

- `"none"` disables cache evictions. Caches will grow without bound. Cache items will still be
  lazily removed if they expire. The persistent data types,
  [`Deque`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Deque "diskcache.Deque") and
  [`Index`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Index "diskcache.Index"),
  use the `"none"` eviction policy. For
  [lazy culling](http://www.grantjenks.com/docs/diskcache/tutorial.html#tutorial-culling) use the
  [cull_limit](http://www.grantjenks.com/docs/diskcache/api.html#constants) setting instead.

All clients accessing the cache are expected to use the same eviction policy. The policy can be set
during initialization using a keyword argument.

```python
>>> cache = Cache()
>>> print(cache.eviction_policy)
least-recently-stored
>>> cache = Cache(eviction_policy='least-frequently-used')
>>> print(cache.eviction_policy)
least-frequently-used
>>> print(cache.reset('eviction_policy', 'least-recently-used'))
least-recently-used
```

Though the eviction policy is changed, the previously created indexes will not be dropped. Prefer to
always specify the eviction policy as a keyword argument to initialize the cache.

## [Disk](http://www.grantjenks.com/docs/diskcache/tutorial.html#id11)[¶](http://www.grantjenks.com/docs/diskcache/tutorial.html#disk "Permalink to this headline")

[`diskcache.Disk`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Disk "diskcache.Disk")
objects are responsible for serializing and deserializing data stored in the cache. Serialization
behavior differs between keys and values. In particular, keys are always stored in the cache
metadata database while values are sometimes stored separately in files.

To customize serialization, you may pass in a
[`Disk`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Disk "diskcache.Disk") subclass
to initialize the cache. All clients accessing the cache are expected to use the same serialization.
The default implementation uses Pickle and the example below uses compressed JSON, available for
convenience as
[`JSONDisk`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.JSONDisk "diskcache.JSONDisk").

```python
import json, zlib

class JSONDisk(diskcache.Disk):
    def__init__(self, directory,compress_level=1, **kwargs):
        self.compress_level = compress_level
        super(JSONDisk, self).__init__(directory, **kwargs)

    def put(self, key):
        json_bytes = json.dumps(key).encode('utf-8')
        data = zlib.compress(json_bytes, self.compress_level)
        return super(JSONDisk, self).put(data)

    def get(self, key, raw):
        data = super(JSONDisk, self).get(key, raw)
        return json.loads(zlib.decompress(data).decode('utf-8'))

    def store(self, value, read):
        if not read:
            json_bytes = json.dumps(value).encode('utf-8')
            value = zlib.compress(json_bytes, self.compress_level)
        return super(JSONDisk, self).store(value, read)

    def fetch(self, mode, filename, value, read):
        data = super(JSONDisk, self).fetch(mode, filename, value, read)
        if not read:
            data = json.loads(zlib.decompress(data).decode('utf-8'))
        return data

with Cache(disk=JSONDisk, disk_compress_level=6) as cache:
    pass
```

Four data types can be stored natively in the cache metadata database: integers, floats, strings,
and bytes. Other datatypes are converted to bytes via the Pickle protocol. Beware that integers and
floats like `1` and `1.0` will compare equal as keys just as in Python. All other equality
comparisons will require identical types.

## [Caveats](http://www.grantjenks.com/docs/diskcache/tutorial.html#id12)[¶](http://www.grantjenks.com/docs/diskcache/tutorial.html#caveats "Permalink to this headline")

Though [DiskCache](http://www.grantjenks.com/docs/diskcache/index.html) has a dictionary-like
interface, Python’s [hash protocol](https://docs.python.org/library/functions.html#hash) is not
used. Neither the <cite>**hash**</cite> nor <cite>**eq**</cite> methods are used for lookups.
Instead lookups depend on the serialization method defined by
[`Disk`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Disk "diskcache.Disk") objects.
For strings, bytes, integers, and floats, equality matches Python’s definition. But large integers
and all other types will be converted to bytes using pickling and the bytes representation will
define equality.

SQLite is used to synchronize database access between threads and processes and as such inherits all
SQLite caveats. Most notably SQLite is [not recommended](https://www.sqlite.org/faq.html#q5) for use
with Network File System (NFS) mounts. For this reason,
[DiskCache](http://www.grantjenks.com/docs/diskcache/index.html) currently
[performs poorly](https://www.pythonanywhere.com/forums/topic/1847/) on
[Python Anywhere](https://www.pythonanywhere.com/). Users have also reported issues running inside
of [Parallels](https://www.parallels.com/) shared folders.

When the disk or database is full, a `sqlite3.OperationalError` will be raised from any method that
attempts to write data. Read operations will still succeed so long as they do not cause any write
(as might occur if cache statistics are being recorded).

Asynchronous support using Python’s `async` and `await` keywords and
[asyncio](https://docs.python.org/3/library/asyncio.html) module is blocked by a lack of support in
the underlying SQLite module. But it is possible to run
[DiskCache](http://www.grantjenks.com/docs/diskcache/index.html) methods in a thread-pool executor
asynchronously. For example:

```python
import asyncioasync def set_async(key, val):
    loop = asyncio.get_running_loop()
    future = loop.run_in_executor(None, cache.set, key, val)
    result = await future
    return result

asyncio.run(set_async('test-key', 'test-value'))
```

The cache
[`volume`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Cache.volume "diskcache.Cache.volume")
is based on the size of the database that stores metadata and the size of the values stored in
files. It does not account the size of directories themselves or other filesystem metadata. If
directory count or size is a concern then consider implementing an alternative
[`Disk`](http://www.grantjenks.com/docs/diskcache/api.html#diskcache.Disk "diskcache.Disk").

## [Implementation](http://www.grantjenks.com/docs/diskcache/tutorial.html#id13)[¶](http://www.grantjenks.com/docs/diskcache/tutorial.html#implementation "Permalink to this headline")

[DiskCache](http://www.grantjenks.com/docs/diskcache/index.html) is mostly built on SQLite and the
filesystem. Some techniques used to improve performance:

- Shard database to distribute writes.

- Leverage SQLite native types: integers, floats, unicode, and bytes.

- Use SQLite write-ahead-log so reads and writes don’t block each other.

- Use SQLite memory-mapped pages to accelerate reads.

- Store small values in SQLite database and large values in files.

- Always use a SQLite index for queries.

- Use SQLite triggers to maintain key count and database size.
