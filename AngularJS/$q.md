## https://code.angularjs.org/1.5.3/docs/api/ng/service/$q

A service that helps you run functions asynchronously, and use their return values (or exceptions) when they are done
processing.

## \$q constructor

The streamlined ES6 style promise is essentially just using \$q as a constructor which takes a resolver function as the
first argument. This is similar to the native Promise implementation from ES6, see MDN.

While the constructor-style use is supported, not all of the supporting methods from ES6 promises are available yet.

It can be used like so:

```js
// for the purpose of this example let's assume that variables `$q` and `okToGreet`
// are available in the current lexical scope (they could have been injected or passed in).

function asyncGreet(name) {
  // perform some asynchronous operation, resolve or reject the promise when appropriate.
  return $q(function(resolve, reject) {
    setTimeout(function() {
      if (okToGreet(name)) {
        resolve("Hello, " + name + "!");
      } else {
        reject("Greeting " + name + " is not allowed.");
      }
    }, 1000);
  });
}

var promise = asyncGreet("Robin Hood");
promise.then(
  function(greeting) {
    alert("Success: " + greeting);
  },
  function(reason) {
    alert("Failed: " + reason);
  }
);
```

Note: progress/notify callbacks are not currently supported via the ES6-style interface.

Note: unlike ES6 behavior, an exception thrown in the constructor function will NOT implicitly reject the promise.

However, the more traditional CommonJS-style usage is still available, and documented below.

The CommonJS Promise proposal describes a promise as an interface for interacting with an object that represents the
result of an action that is performed asynchronously, and may or may not be finished at any given point in time.

From the perspective of dealing with error handling, deferred and promise APIs are to asynchronous programming what
`try, catch and throw` keywords are to synchronous programming.

```js
// for the purpose of this example let's assume that variables `$q` and `okToGreet`
// are available in the current lexical scope (they could have been injected or passed in).

function asyncGreet(name) {
  var deferred = $q.defer();

  setTimeout(function() {
    deferred.notify("About to greet " + name + ".");

    if (okToGreet(name)) {
      deferred.resolve("Hello, " + name + "!");
    } else {
      deferred.reject("Greeting " + name + " is not allowed.");
    }
  }, 1000);

  return deferred.promise;
}

var promise = asyncGreet("Robin Hood");
promise.then(
  function(greeting) {
    alert("Success: " + greeting);
  },
  function(reason) {
    alert("Failed: " + reason);
  },
  function(update) {
    alert("Got notification: " + update);
  }
);
```

At first it might not be obvious why this extra complexity is worth the trouble. The payoff comes in the way of
guarantees that promise and deferred APIs make, see
https://github.com/kriskowal/uncommonjs/blob/master/promises/specification.md.

Additionally the promise api allows for composition that is very hard to do with the traditional callback (CPS)
approach. For more on this please see the Q documentation especially the section on serial or parallel joining of
promises.

## The Deferred API

A new instance of deferred is constructed by calling \$q.defer().

The purpose of the deferred object is to expose the associated Promise instance as well as APIs that can be used for
signaling the successful or unsuccessful completion, as well as the status of the task.

### Methods

- resolve(value) – resolves the derived promise with the value. If the value is a rejection constructed via \$q.reject,
  the promise will be rejected instead.
- reject(reason) – rejects the derived promise with the reason. This is equivalent to resolving it with a rejection
  constructed via \$q.reject.
- notify(value) - provides updates on the status of the promise's execution. This may be called multiple times before
  the promise is either resolved or rejected.

### Properties

- promise – {Promise} – promise object associated with this deferred.
