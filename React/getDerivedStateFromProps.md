# Replacing ‘componentWillReceiveProps’ with ‘getDerivedStateFromProps’

https://hackernoon.com/replacing-componentwillreceiveprops-with-getderivedstatefromprops-c3956f7ce607

With the release of React 16.3, some new lifecycle methods have been introduced, and release of React 17 will deprecate
some lifecycle method.

`getDerivedStateFromProps` is one of those newly introduced lifecycle method replacing `componentWillReceiveProps`,
which has now become `UNSAFE_componentWillReceiveProps`.

`getDerivedStateFromProps` is a static method which is invoked after a component is instantiated as well as when it
receives new props. Since it is a static method, you cannot access this inside this method neither you can access any
other class method. Unlike `componentWillReceiveProps` you cannot set state inside this method, so the only way to
update state is returning an object. If you don’t want to update any state, simply return null.

## Let’s dive into some code

This is how `componentWillReceiveProps` works.

```jsx
componentWillReceiveProps(nextProps){
  if(nextProps.someValue!==this.props.someValue){
    //Perform some operation
    this.setState({someState: someValue });
    this.classMethod();
  }
}
```

We compare `nextProps.someValue` with `this.props.someValue` and if both are different then we perform some operation,
`setState` and call `this.classMethod()`;.

Now let’s have a look how `getDerivedStateFromProps` works.

```jsx
static getDerivedStateFromProps(nextProps, prevState){
   if(nextProps.someValue!==prevState.someValue){
     return { someState: nextProps.someValue};
  }
  else return null;
}

componentDidUpdate(prevProps, prevState) {
  if(prevProps.someValue!==this.props.someValue){
    //Perform some operation here
    this.setState({someState: someValue});
    this.classMethod();
  }
}
```

It receives two params `nextProps` and `prevState`. As mentioned previously you cannot access this inside this method so
you’ll have to store the props in the state to compare the `nextProps` with previous props. In above code `nextProps`
and `prevState` are compared, if both are different then an object will be returned to update the state otherwise null
will be returned indicating state update not required. If state changes then `componentDidUpdate` is called where we can
perform the desired operations as we did in `componentWillReceiveProps`.
