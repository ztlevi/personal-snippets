# Interfaces

Let’s develop our sample further. Here we use an interface that describes objects that have a firstName and lastName
field. In TypeScript, two types are compatible if their internal structure is compatible. This allows us to implement an
interface just by having the shape the interface requires, without an explicit `implements` clause.

```typescript
interface Person {
  firstName: string;
  lastName: string;
}

function greeter(person: Person) {
  return "Hello, " + person.firstName + " " + person.lastName;
}

let user = { firstName: "Jane", lastName: "User" };

document.body.innerHTML = greeter(user);
```
