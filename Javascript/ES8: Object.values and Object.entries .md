The obj parameter is the source object for the operation. It can be an object or an array (that is an object with
indexes like [10, 20, 30] -> { 0: 10, 1: 20, 2: 30 }).

```
const obj = { x: 'xxx', y: 1 };
Object.values(obj); // ['xxx', 1]

const obj = ['e', 's', '8']; // same as { 0: 'e', 1: 's', 2: '8' };
Object.values(obj); // ['e', 's', '8']

// when we use numeric keys, the values returned in a numerical
// order according to the keys
const obj = { 10: 'xxx', 1: 'yyy', 3: 'zzz' };
Object.values(obj); // ['yyy', 'zzz', 'xxx']
Object.values('es8'); // ['e', 's', '8']
```

The Object.entries method returns an array of a given object's own enumerable property [key, value] pairs, in the same
order as Object.values. The declaration of the function is trivial:

```
const obj = { x: 'xxx’, y: 1 };
Object.entries(obj); // [[’x’, 'xxx’], [’y’, 1]]

const obj = [’e’, 's’, '8’];
Object.entries(obj); // [[’0’, 'e’], [’1’, 's’], [’2’, '8’]]

const obj = { 10: 'xxx’, 1: 'yyy’, 3: 'zzz' };
Object.entries(obj); // [[’1’, 'yyy’], [’3’, 'zzz’], [’10’, 'xxx’]]
Object.entries('es8'); // [['0', 'e'], ['1', 's'], ['2', '8']]
```
