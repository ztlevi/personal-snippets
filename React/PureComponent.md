# How does `PureComponent` work?

http://lucybain.com/blog/2018/react-js-pure-component/

Ok, you’ve had your big reveal. How does it actually work?

You know how we’d normally need to write our own `shouldComponentUpdate` to check if the component
should re-render or not? Well, React has written that check for us in `PureComponent`. The relevant
`shouldComponentUpdate` code is:

```jsx
if (type.prototype && type.prototype.isPureReactComponent) {
  shouldUpdate = !shallowEqual(oldProps, props) || !shallowEqual(oldState, state);
}
```

> **Note**: React checks both props and state. Throughout this article I focus on state because it
> makes the examples easier and self contained, however it’s important to note that everything we
> talk about here equally applies to props.

Here is the code for that shallowEqual function. Of particular interest is the method documentation:

> Performs equality by iterating through keys on an object and returning false when any key has
> values which are not strictly equal between the arguments. Returns true when the values of all
> keys are strictly equal.

But wait, what does “strictly equal” mean?

That is a very good question, so we’ll dedicate a whole section to it.

# Warning: PureComponent does a shallow equality check

# Wrapping up

`PureComponent` is very powerful in that it can help you limit the number of unnecessary re-renders
that occur. However, it can also cause surprising gotchas. The key thing to keep in mind is that
`PureComponent` only does a shallow equality check on props and state before deciding if it should
re-render or not. And that has a cascade effect on if its children re-render or not. So use
`PureComponent` and love the performance gains, but be sure to check that it is always re-rendering
when it should. When in doubt fall back to a `Component` instead.
