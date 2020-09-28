```
Object.getOwnPropertyDescriptors(obj)
```

The obj is the source object. The possible keys for the returned descriptor objects result are
configurable, enumerable, writable, get, set and value.

```
const obj = {
  get es7() { return 777; },
  get es8() { return 888; }
};
Object.getOwnPropertyDescriptors(obj);
// {
//   es7: {
//     configurable: true,
//     enumerable: true,
//     get: function es7(){}, //the getter function
//     set: undefined
//   },
//   es8: {
//     configurable: true,
//     enumerable: true,
//     get: function es8(){}, //the getter function
//     set: undefined
//   }
// }
```

The descriptor data is very important for
[advanced features like decorators](https://hackernoon.com/all-you-need-to-know-about-decorators-a-case-study-4a7e776b22a6).
