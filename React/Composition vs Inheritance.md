## https://reactjs.org/docs/composition-vs-inheritance.html

# While this is less common, sometimes you might need multiple “holes” in a component. In such cases you may come up with your own convention instead of using children:

```js
function SplitPane(props) {
  return (
    <div className="SplitPane">
      <div className="SplitPane-left">{props.left}</div>
      <div className="SplitPane-right">{props.right}</div>
    </div>
  );
}

function App() {
  return <SplitPane left={<Contacts />} right={<Chat />} />;
}
```

# So What About Inheritance?

At Facebook, we use React in thousands of components, and we haven’t found any use cases where we would recommend
creating component inheritance hierarchies.

Props and composition give you all the flexibility you need to customize a component’s look and behavior in an explicit
and safe way. Remember that components may accept arbitrary props, including primitive values, React elements, or
functions.

If you want to reuse non-UI functionality between components, we suggest extracting it into a separate JavaScript
module. The components may import it and use that function, object, or a class, without extending it.
