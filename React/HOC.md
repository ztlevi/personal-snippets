# Definition

Concretely, a higher-order component is a function that takes a component and returns a new component.

HOCs are common in third-party React libraries, such as Redux’s connect and Relay’s createFragmentContainer.

# Don't Use HOCs Inside the render Method

https://github.com/facebook/react/blob/044015760883d03f060301a15beef17909abbf71/docs/docs/higher-order-components.md#dont-use-hocs-inside-the-render-method

React's diffing algorithm (called reconciliation) uses component identity to determine whether it should update the
existing subtree or throw it away and mount a new one. If the component returned from render is identical (===) to the
component from the previous render, React recursively updates the subtree by diffing it with the new one. If they're not
equal, the previous subtree is unmounted completely.

Normally, you shouldn't need to think about this. But it matters for HOCs because it means you can't apply an HOC to a
component within the render method of a component:

```javascript
render() {
  // A new version of EnhancedComponent is created on every render
  // EnhancedComponent1 !== EnhancedComponent2
  const EnhancedComponent = enhance(MyComponent);
  // That causes the entire subtree to unmount/remount each time!
  return <EnhancedComponent />;
}
```

The problem here isn't just about performance — remounting a component causes the state of that component and all of its
children to be lost.

Instead, apply HOCs outside the component definition so that the resulting component is created only once. Then, its
identity will be consistent across renders. This is usually what you want, anyway.

In those rare cases where you need to apply an HOC dynamically, you can also do it inside a component's lifecycle
methods or its constructor.
