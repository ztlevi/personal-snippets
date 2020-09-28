## https://github.com/angular/angular-phonecat/compare/step-2...step-3

To create a component, we use the .component() method of an AngularJS module. We must provide the
name of the component and the Component Definition Object (CDO for short).

Remember that (since components are also directives) the name of the component is in camelCase (e.g.
myAwesomeComponent), but we will use kebab-case (e.g. my-awesome-component) when referring to it in
our HTML. (See here for a description of different case styles.)

In its simplest form, the CDO will just contain a template and a controller. (We can actually omit
the controller and AngularJS will create a dummy controller for us. This is useful for simple
"presentational" components, that don't attach any behavior to the template.)

Let's see an example:

```js
angular.module("myApp").component("greetUser", {
  template: "Hello, {{$ctrl.user}}!",
  controller: function GreetUserController() {
    this.user = "world";
  },
});
```

Now, every time we include <greet-user></greet-user> in our view, AngularJS will expand it into a
DOM sub-tree constructed using the provided template and managed by an instance of the specified
controller.
