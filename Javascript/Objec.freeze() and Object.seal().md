The Object.freeze() method freezes an object: that is, prevents new properties from being added to
it; prevents existing properties from being removed; and prevents existing properties, or their
enumerability, configurability, or writability, from being changed, it also prevents the prototype
from being changed. The method returns the passed object.

```
const object1 = {
  property1: 42
};

const object2 = Object.freeze(object1);

object2.property1 = 33;
// Throws an error in strict mode

console.log(object2.property1);
// expected output: 42
```

# Seal

The Object.seal() method seals an object, preventing new properties from being added to it and
marking all existing properties as non-configurable. Values of present properties can still be
changed as long as they are writable.

```
const object1 = {
  property1: 42
};

Object.seal(object1);
object1.property1 = 33;
console.log(object1.property1);
// expected output: 33

delete object1.property1; // cannot delete when sealed
console.log(object1.property1);
// expected output: 33
```
