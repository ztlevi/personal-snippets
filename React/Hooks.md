# Hooks: https://reactjs.org/hooks

### State Hook

```jsx
import { useState } from "react";

function Example() {
  // Declare a new state variable, which we'll call "count"
  const [count, setCount] = useState(0);

  return (
    <div>
      <p>You clicked {count} times</p>
      <button onClick={() => setCount(count + 1)}>Click me</button>
    </div>
  );
}
```

### Effect Hook

Let’s see how we could write this component with Hooks.

You might be thinking that we’d need a separate effect to perform the cleanup. But code for adding and removing a
subscription is so tightly related that `useEffect` is designed to keep it together. If your effect returns a function,
React will run it when it is time to clean up:

```jsx
import { useState, useEffect } from "react";

function FriendStatus(props) {
  const [isOnline, setIsOnline] = useState(null);

  function handleStatusChange(status) {
    setIsOnline(status.isOnline);
  }

  useEffect(() => {
    ChatAPI.subscribeToFriendStatus(props.friend.id, handleStatusChange);
    // Specify how to clean up after this effect:
    return function cleanup() {
      ChatAPI.unsubscribeFromFriendStatus(props.friend.id, handleStatusChange);
    };
  });

  if (isOnline === null) {
    return "Loading...";
  }
  return isOnline ? "Online" : "Offline";
}
```

**Why did we return a function from our effect?** This is the optional cleanup mechanism for effects. Every effect may
return a function that cleans up after it. This lets us keep the logic for adding and removing subscriptions close to
each other. They’re part of the same effect!

**When exactly does React clean up an effect?** React performs the cleanup when the component unmounts. However, as we
learned earlier, effects run for every render and not just once. This is why React _also_ cleans up effects from the
previous render before running the effects next time. We’ll discuss
[why this helps avoid bugs](https://reactjs.org/docs/hooks-effect.html#explanation-why-effects-run-on-each-update) and
[how to opt out of this behavior in case it creates performance issues](https://reactjs.org/docs/hooks-effect.html#tip-optimizing-performance-by-skipping-effects)
later below.

> Note
>
> We don’t have to return a named function from the effect. We called it `cleanup` here to clarify its purpose, but you
> could return an arrow function or call it something different.

#### Tip: Optimizing Performance by Skipping Effects

In some cases, cleaning up or applying the effect after every render might create a performance problem. In class
components, we can solve this by writing an extra comparison with `prevProps` or `prevState` inside
`componentDidUpdate`:

```gatsby-code-jsx
componentDidUpdate(prevProps, prevState) {
  if (prevState.count !== this.state.count) {
    document.title = `You clicked ${this.state.count} times`;
  }
}
```

This requirement is common enough that it is built into the `useEffect` Hook API. You can tell React to _skip_ applying
an effect if certain values haven’t changed between re-renders. To do so, pass an array as an optional second argument
to `useEffect`:

```gatsby-code-jsx
useEffect(() => {
  document.title = `You clicked ${count} times`;
}, [count]); // Only re-run the effect if count changes
```

### `useRef`

```gatsby-code-jsx
const refContainer = useRef(initialValue);
```

`useRef` returns a mutable ref object whose `.current` property is initialized to the passed argument (`initialValue`).
The returned object will persist for the full lifetime of the component.

A common use case is to access a child imperatively:

```gatsby-code-jsx
function TextInputWithFocusButton() {
  const inputEl = useRef(null);
  const onButtonClick = () => {
    // `current` points to the mounted text input element
    inputEl.current.focus();
  };
  return (
    <>
      <input ref={inputEl} type="text" />
      <button onClick={onButtonClick}>Focus the input</button>
    </>
  );
}
```

Note that `useRef()` is useful for more than the `ref` attribute. It’s
[handy for keeping any mutable value around](https://reactjs.org/docs/hooks-faq.html#is-there-something-like-instance-variables)
similar to how you’d use instance fields in classes.

## usePreferredColorScheme.js for MacOS Mojave themes

https://github.com/rhysforyou/hooktopia/blob/master/src/hooks/usePreferredColorScheme.js

```jsx
import { useState, useEffect } from "react";

export default function usePreferredColorScheme(defaultColorScheme = "light") {
  const [matches, setMatches] = useState(defaultColorScheme == "dark");

  useEffect(() => {
    const mediaQueryList = window.matchMedia("(prefers-color-scheme: dark)");
    let active = true;

    const listener = () => {
      if (!active) {
        return;
      }

      setMatches(mediaQueryList.matches);
    };

    mediaQueryList.addListener(listener);
    setMatches(mediaQueryList.matches);

    return () => {
      active = false;
      mediaQueryList.removeListener(listener);
    };
  }, []);

  return matches ? "dark" : "light";
}
```
