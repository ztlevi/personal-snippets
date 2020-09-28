## 2.2 Declarative Effect

https://redux-saga.js.org/docs/basics/DeclarativeEffects.html

`call` also supports invoking object methods, you can provide a `this` context to the invoked
functions using the following form:

```javascript
yield call([obj, obj.method], arg1, arg2, ...) // as if we did obj.method(arg1, arg2 ...)
```

`apply` is an alias for the method invocation form

```javascript
yield apply(obj, obj.method, [arg1, arg2, ...])
```

`call` and `apply` are well suited for functions that return Promise results. Another function `cps`
can be used to handle Node style functions (e.g. `fn(...args, callback)` where `callback` is of the
form `(error, result) => ())`. `cps` stands for Continuation Passing Style.

For example:

```javascript
import { cps } from 'redux-saga/effects'

const content = yield cps(readFile, '/path/to/file')
```

And of course you can test it just like you test call:

```javascript
import { cps } from "redux-saga/effects";

const iterator = fetchSaga();
assert.deepEqual(iterator.next().value, cps(readFile, "/path/to/file"));
```

`cps` also supports the same method invocation form as `call`.

## 2.3 Dispatching actions to the store

We could pass the Store's `dispatch` function to the Generator. Then the Generator could invoke it
after receiving the fetch response:

```javascript
// ...

function* fetchProducts(dispatch) {
  const products = yield call(Api.fetch, "/products");
  dispatch({ type: "PRODUCTS_RECEIVED", products });
}
```

However, this solution has the same drawbacks as invoking functions directly from inside the
Generator (as discussed in the previous section). If we want to test that `fetchProducts` performs
the `dispatch` after receiving the AJAX response, we'll need again to mock the dispatch function.

Instead, we need the same declarative solution. Just create an Object to instruct the middleware
that we need to dispatch some action, and let the middleware perform the real dispatch. This way we
can test the Generator's dispatch in the same way: by just inspecting the yielded Effect and making
sure it contains the correct instructions.

The library provides, for this purpose, another function `put` which creates the dispatch Effect.

```javascript
import { call, put } from "redux-saga/effects";
// ...

function* fetchProducts() {
  const products = yield call(Api.fetch, "/products");
  // create and yield a dispatch Effect
  yield put({ type: "PRODUCTS_RECEIVED", products });
}
```

Now, we can test the Generator easily as in the previous section

```javascript
import { call, put } from "redux-saga/effects";
import Api from "...";

const iterator = fetchProducts();

// expects a call instruction
assert.deepEqual(
  iterator.next().value,
  call(Api.fetch, "/products"),
  "fetchProducts should yield an Effect call(Api.fetch, './products')"
);

// create a fake response
const products = {};

// expects a dispatch instruction
assert.deepEqual(
  iterator.next(products).value,
  put({ type: "PRODUCTS_RECEIVED", products }),
  "fetchProducts should yield an Effect put({ type: 'PRODUCTS_RECEIVED', products })"
);
```

## 2.4 Error Handling

We want to handle those errors inside our Saga by dispatching a `PRODUCTS_REQUEST_FAILED` action to
the Store.

We can catch errors inside the Saga using the familiar `try/catch` syntax.

```javascript
import Api from "./path/to/api";
import { call, put } from "redux-saga/effects";

// ...

function* fetchProducts() {
  try {
    const products = yield call(Api.fetch, "/products");
    yield put({ type: "PRODUCTS_RECEIVED", products });
  } catch (error) {
    yield put({ type: "PRODUCTS_REQUEST_FAILED", error });
  }
}
```

Of course, you're not forced to handle your API errors inside `try/catch` blocks. You can also make
your API service return a normal value with some error flag on it. For example, you can catch
Promise rejections and map them to an object with an error field.

```javascript
import Api from "./path/to/api";
import { call, put } from "redux-saga/effects";

function fetchProductsApi() {
  return Api.fetch("/products")
    .then((response) => ({ response }))
    .catch((error) => ({ error }));
}

function* fetchProducts() {
  const { response, error } = yield call(fetchProductsApi);
  if (response) yield put({ type: "PRODUCTS_RECEIVED", products: response });
  else yield put({ type: "PRODUCTS_REQUEST_FAILED", error });
}
```

In order to test the failure case, we'll use the `throw` method of the Generator

```javascript
import { call, put } from "redux-saga/effects";
import Api from "...";

const iterator = fetchProducts();

// expects a call instruction
assert.deepEqual(
  iterator.next().value,
  call(Api.fetch, "/products"),
  "fetchProducts should yield an Effect call(Api.fetch, './products')"
);

// create a fake error
const error = {};

// expects a dispatch instruction
assert.deepEqual(
  iterator.throw(error).value,
  put({ type: "PRODUCTS_REQUEST_FAILED", error }),
  "fetchProducts should yield an Effect put({ type: 'PRODUCTS_REQUEST_FAILED', error })"
);
```
