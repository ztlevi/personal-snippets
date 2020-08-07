# Event object

https://javascript.info/introduction-browser-events#event-object

To properly handle an event we’d want to know more about what’s happened. Not just a “click” or a “keypress”, but what
were the pointer coordinates? Which key was pressed? And so on.

When an event happens, the browser creates an event object, puts details into it and passes it as an argument to the
handler.

Here’s an example of getting mouse coordinates from the event object:

```html
<input type="button" value="Click me" id="elem" />

<script>
  elem.onclick = function(event) {
    // show event type, element and coordinates of the click
    alert(event.type + " at " + event.currentTarget);
    alert("Coordinates: " + event.clientX + ":" + event.clientY);
  };
</script>
```

Some properties of `event` object:

- `event.type` Event type, here it’s `"click"`.
- `event.currentTarget` Element that handled the event. That’s exactly the same as `this`, unless you bind `this` to
  something else, and then `event.currentTarget` becomes useful.
- `event.clientX / event.clientY` Window-relative coordinates of the cursor, for mouse events.

  There are more properties. They depend on the event type, so we’ll study them later when come to different events in
  details.

# Object handlers: handleEvent

We can assign an object as an event handler using `addEventListener`. When an event occurs, its `handleEvent` method is
called with it.

For instance:

```html
<button id="elem">Click me</button>

<script>
  elem.addEventListener("click", {
    handleEvent(event) {
      alert(event.type + " at " + event.currentTarget);
    },
  });
</script>
```

In other words, when `addEventListener` receives an object as the handler, it calls `object.handleEvent(event)` in case
of an event.

We could also use a class for that:

```html
<button id="elem">Click me</button>

<script>
  class Menu {
    handleEvent(event) {
      switch (event.type) {
        case "mousedown":
          elem.innerHTML = "Mouse button pressed";
          break;
        case "mouseup":
          elem.innerHTML += "...and released.";
          break;
      }
    }
  }

  let menu = new Menu();
  elem.addEventListener("mousedown", menu);
  elem.addEventListener("mouseup", menu);
</script>
```

Here the same object handles both events. Please note that we need to explicitly setup the events to listen using
`addEventListener`. The `menu` object only gets `mousedown` and `mouseup` here, not any other types of events.

The method `handleEvent` does not have to do all the job by itself. It can call other event-specific methods instead,
like this:

```html
<button id="elem">Click me</button>

<script>
  class Menu {
    handleEvent(event) {
      // mousedown -> onMousedown
      let method = "on" + event.type[0].toUpperCase() + event.type.slice(1);
      this[method](event);
    }

    onMousedown() {
      elem.innerHTML = "Mouse button pressed";
    }

    onMouseup() {
      elem.innerHTML += "...and released.";
    }
  }

  let menu = new Menu();
  elem.addEventListener("mousedown", menu);
  elem.addEventListener("mouseup", menu);
</script>
```

Now event handlers are clearly separated, that may be easier to support.
