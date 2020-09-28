# React Loadable

https://reactjs.org/docs/code-splitting.html#react-loadable

React Loadable wraps dynamic imports in a nice, React-friendly API for introducing code splitting
into your app at a given component.

- Before

```jsx
import OtherComponent from "./OtherComponent";

const MyComponent = () => <OtherComponent />;
```

- After

```jsx
import Loadable from "react-loadable";

const LoadableOtherComponent = Loadable({
  loader: () => import("./OtherComponent"),
  loading: () => <div>Loading...</div>,
});

const MyComponent = () => <LoadableOtherComponent />;
```

React Loadable helps you create loading states, error states, timeouts, preloading, and more. It can
even help you server-side render an app with lots of code-splitting.
