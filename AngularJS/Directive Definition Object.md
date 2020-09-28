## https://code.angularjs.org/1.5.3/docs/api/ng/service/$compile

## Here's an example directive declared with a Directive Definition Object:

```js
var myModule = angular.module(...);

myModule.directive('directiveName', function factory(injectables) {
  var directiveDefinitionObject = {
    priority: 0,
    template: '<div></div>', // or // function(tElement, tAttrs) { ... },
    // or
    // templateUrl: 'directive.html', // or // function(tElement, tAttrs) { ... },
    transclude: false,
    restrict: 'A',
    templateNamespace: 'html',
    scope: false,
    controller: function($scope, $element, $attrs, $transclude, otherInjectables) { ... },
    controllerAs: 'stringIdentifier',
    bindToController: false,
    require: 'siblingDirectiveName', // or // ['^parentDirectiveName', '?optionalDirectiveName', '?^optionalParent'],
    compile: function compile(tElement, tAttrs, transclude) {
      return {
        pre: function preLink(scope, iElement, iAttrs, controller) { ... },
        post: function postLink(scope, iElement, iAttrs, controller) { ... }
      }
      // or
      // return function postLink( ... ) { ... }
    },
    // or
    // link: {
    //  pre: function preLink(scope, iElement, iAttrs, controller) { ... },
    //  post: function postLink(scope, iElement, iAttrs, controller) { ... }
    // }
    // or
    // link: function postLink( ... ) { ... }
  };
  return directiveDefinitionObject;
});
```

## Directive Definition Object

The directive definition object provides instructions to the compiler. The attributes are:

### multiElement

When this property is set to true, the HTML compiler will collect DOM nodes between nodes with the
attributes `directive-name-start` and `directive-name-end`, and group them together as the directive
elements. It is recommended that this feature be used on directives which are not strictly
behavioral (such as ngClick), and which do not manipulate or replace child nodes (such as
ngInclude).

### priority

When there are multiple directives defined on a single DOM element, sometimes it is necessary to
specify the order in which the directives are applied. The priority is used to sort the directives
before their compile functions get called. Priority is defined as a number. Directives with greater
numerical priority are compiled first. Pre-link functions are also run in priority order, but
post-link functions are run in reverse order. The order of directives with the same priority is
undefined. The default priority is 0.

### terminal

If set to true then the current priority will be the last set of directives which will execute (any
directives at the current priority will still execute as the order of execution on same priority is
undefined). Note that expressions and other directives used in the directive's template will also be
excluded from execution.

### scope

The scope property can be true, an object or a falsy value:

- falsy: No scope will be created for the directive. The directive will use its parent's scope.

- true: A new child scope that prototypically inherits from its parent will be created for the
  directive's element. If multiple directives on the same element request a new scope, only one new
  scope is created. The new scope rule does not apply for the root of the template since the root of
  the template always gets a new scope.

- {...} (an object hash): A new "isolate" scope is created for the directive's element. The
  'isolate' scope differs from normal scope in that it does not prototypically inherit from its
  parent scope. This is useful when creating reusable components, which should not accidentally read
  or modify data in the parent scope.

The 'isolate' scope object hash defines a set of local scope properties derived from attributes on
the directive's element. These local properties are useful for aliasing values for templates. The
keys in the object hash map to the name of the property on the isolate scope; the values define how
the property is bound to the parent scope, via matching attributes on the directive's element:

- @ or @attr - bind a local scope property to the value of DOM attribute. The result is always a
  string since DOM attributes are strings. If no attr name is specified then the attribute name is
  assumed to be the same as the local name. Given `<my-component my-attr="hello {{name}}">` and the
  isolate scope definition `scope: { localName:'@myAttr' }`, the directive's scope property
  localName will reflect the interpolated value of `hello {{name}}`. As the name attribute changes
  so will the localName property on the directive's scope. The name is read from the parent scope
  (not the directive's scope).

- = or =attr - set up a bidirectional binding between a local scope property and an expression
  passed via the attribute attr. The expression is evaluated in the context of the parent scope. If
  no attr name is specified then the attribute name is assumed to be the same as the local name.
  Given `<my-component my-attr="parentModel">` and the isolate scope definition
  `scope: { localModel: '=myAttr' }`, the property localModel on the directive's scope will reflect
  the value of parentModel on the parent scope. Changes to parentModel will be reflected in
  localModel and vice versa. Optional attributes should be marked as such with a question mark: =?
  or =?attr. If the binding expression is non-assignable, or if the attribute isn't optional and
  doesn't exist, an exception
  ($compile:nonassign) will be thrown upon discovering changes to the local value, since it will be impossible to sync them back to the parent scope. By default, the $watch
  method is used for tracking changes, and the equality check is based on object identity. However,
  if an object literal or an array literal is passed as the binding expression, the equality check
  is done by value (using the angular.equals function). It's also possible to watch the evaluated
  value shallowly with \$watchCollection: use `=*` or `=*attr` (`=*?` or `=*?attr` if the attribute
  is optional).

- `<` or `<attr` - set up a one-way (one-directional) binding between a local scope property and an
  expression passed via the attribute attr. The expression is evaluated in the context of the parent
  scope. If no attr name is specified then the attribute name is assumed to be the same as the local
  name. You can also make the binding optional by adding `?`: `<?` or `<?attr`.

For example, given `<my-component my-attr="parentModel">` and directive definition of
`scope: { localModel:'<myAttr' }`, then the isolated scope property localModel will reflect the
value of parentModel on the parent scope. Any changes to parentModel will be reflected in
localModel, but changes in localModel will not reflect in parentModel. There are however two
caveats:

    - one-way binding does not copy the value from the parent to the isolate scope, it simply sets the same value. That means if your bound value is an object, changes to its properties in the isolated scope will be reflected in the parent scope (because both reference the same object).
    - one-way binding watches changes to the identity of the parent value. That means the $watch on the parent value only fires if the reference to the value has changed. In most cases, this should not be of concern, but can be important to know if you one-way bind to an object, and then replace that object in the isolated scope. If you now change a property of the object in your parent scope, the change will not be propagated to the isolated scope, because the identity of the object on the parent scope has not changed. Instead you must assign a new object.

One-way binding is useful if you do not plan to propagate changes to your isolated scope bindings
back to the parent. However, it does not make this completely impossible.

- & or &attr - provides a way to execute an expression in the context of the parent scope. If no
  attr name is specified then the attribute name is assumed to be the same as the local name. Given
  `<my-component my-attr="count = count + value">` and the isolate scope definition
  `scope: { localFn:'&myAttr' }`, the isolate scope property localFn will point to a function
  wrapper for the count = count + value expression. Often it's desirable to pass data from the
  isolated scope via an expression to the parent scope. This can be done by passing a map of local
  variable names and values into the expression wrapper fn. For example, if the expression is
  increment(amount) then we can specify the amount value by calling the localFn as
  `localFn({amount: 22})`.

In general it's possible to apply more than one directive to one element, but there might be
limitations depending on the type of scope required by the directives. The following points will
help explain these limitations. For simplicity only two directives are taken into account, but it is
also applicable for several directives:

- no scope + no scope => Two directives which don't require their own scope will use their parent's
  scope child scope + no scope => Both directives will share one single child scope
- child scope + child scope => Both directives will share one single child scope
- isolated scope + no scope => The isolated directive will use it's own created isolated scope. The
  other directive will use its parent's scope
- isolated scope + child scope => Won't work! Only one scope can be related to one element.
  Therefore these directives cannot be applied to the same element.
- isolated scope + isolated scope => Won't work! Only one scope can be related to one element.
  Therefore these directives cannot be applied to the same element.

### restrict

String of subset of EACM which restricts the directive to a specific directive declaration style. If
omitted, the defaults (elements and attributes) are used.

- E - Element name (default): `<my-directive></my-directive>`
- A - Attribute (default): `<div my-directive="exp"></div>`
- C - Class: `<div class="my-directive: exp;"></div>`
- M - Comment: `<!-- directive: my-directive exp -->`
