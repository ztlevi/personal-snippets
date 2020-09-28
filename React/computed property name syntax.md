## https://reactjs.org/docs/forms.html#handling-multiple-inputs

ES6 computed property name syntax to update the state key corresponding to the given input name:

```js
this.setState({
  [name]: value,
});
```

It is equivalent to this ES5 code:

```js
var partialState = {};
partialState[name] = value;
this.setState(partialState);
```

Also, since `setState()` automatically merges a partial state into the current state, we only needed
to call it with the changed parts.
