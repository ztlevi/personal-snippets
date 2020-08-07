# Setup and Teardown

https://jasmine.github.io/2.0/introduction.html

To help a test suite DRY up any duplicated setup and teardown code, Jasmine provides the global `beforeEach` and
`afterEach` functions. As the name implies, the `beforeEach` function is called once before each spec in the describe in
which it is called, and the `afterEach` function is called once after each spec. Here is the same set of specs written a
little differently. The variable under test is defined at the top-level scope -- the `describe` block -- and
initialization code is moved into a `beforeEach` function. The `afterEach` function resets the variable before
continuing.

```jsx
describe("A spec (with setup and tear-down)", function() {
  var foo;

  beforeEach(function() {
    foo = 0;
    foo += 1;
  });

  afterEach(function() {
    foo = 0;
  });

  it("is just a function, so it can contain any code", function() {
    expect(foo).toEqual(1);
  });

  it("can have more than one expectation", function() {
    expect(foo).toEqual(1);
    expect(true).toEqual(true);
  });
});
```

# Disabling Suites

Suites can be disabled with the `xdescribe` function. These suites and any specs inside them are skipped when run and
thus their results will not appear in the results.

# Pending Specs

Pending specs do not run, but their names will show up in the results as pending.

Any spec declared with xit is marked as pending.

Any spec declared without a function body will also be marked pending in results.

And if you call the function pending anywhere in the spec body, no matter the expectations, the spec will be marked
pending.

```jsx
describe("Pending specs", function() {
  xit("can be declared 'xit'", function() {
    expect(true).toBe(false);
  });

  it("can be declared with 'it' but without a function");

  it("can be declared by calling 'pending' in the spec body", function() {
    expect(true).toBe(false);
    pending();
  });
});
```
