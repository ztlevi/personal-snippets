# Route-based code splitting

https://reactjs.org/docs/code-splitting.html#route-based-code-splitting

Deciding where in your app to introduce code splitting can be a bit tricky. You want to make sure you choose places that
will split bundles evenly, but won’t disrupt the user experience.

A good place to start is with routes. Most people on the web are used to page transitions taking some amount of time to
load. You also tend to be re-rendering the entire page at once so your users are unlikely to be interacting with other
elements on the page at the same time.

Here’s an example of how to setup route-based code splitting into your app using libraries like React Router and React
Loadable.

```jsx
import { BrowserRouter as Router, Route, Switch } from "react-router-dom";
import Loadable from "react-loadable";

const Loading = () => <div>Loading...</div>;

const Home = Loadable({
  loader: () => import("./routes/Home"),
  loading: Loading,
});

const About = Loadable({
  loader: () => import("./routes/About"),
  loading: Loading,
});

const App = () => (
  <Router>
    <Switch>
      <Route exact path="/" component={Home} />
      <Route path="/about" component={About} />
    </Switch>
  </Router>
);
```
