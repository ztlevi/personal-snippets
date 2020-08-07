## https://code.angularjs.org/1.5.3/docs/api/ng/directive/ngModel

## Introduction

The ngModel directive binds an input,select, textarea (or custom form control) to a property on the scope using
NgModelController, which is created and exposed by this directive.

ngModel is responsible for:

- Binding the view into the model, which other directives such as input, textarea or select require.

- Providing validation behavior (i.e. required, number, email, url).

- Keeping the state of the control (valid/invalid, dirty/pristine, touched/untouched, validation errors).

- Setting related css classes on the element (ng-valid, ng-invalid, ng-dirty, ng-pristine, ng-touched, ng-untouched,
  ng-empty, ng-not-empty) including animations.

- Registering the control with its parent form.

> Note: ngModel will try to bind to the property given by evaluating the expression on the current scope. If the
> property doesn't already exist on this scope, it will be created implicitly and added to the scope.

## Always have a '.' in your ng-models

New AngularJS developers often do not realize that ng-repeat, ng-switch, ng-view, ng-include and ng-if all create new
child scopes, so the problem often shows up when these directives are involved. (See this example for a quick
illustration of the problem.)

This issue with primitives can be easily avoided by following the "best practice" of
[always have a '.' in your ng-models](https://www.youtube.com/watch?v=ZhfUv0spHCY&feature=youtu.be&t=30m) – watch 3
minutes worth. Misko demonstrates the primitive binding issue with ng-switch.

Having a '.' in your models will ensure that prototypal inheritance is in play. So, use

`<input type="text" ng-model="someObj.prop1">` rather than

`<input type="text" ng-model="prop1">`.

If you really want/need to use a primitive, there are two workarounds:

Use \$parent.parentScopeProperty in the child scope. This will prevent the child scope from creating its own property.

Define a function on the parent scope, and call it from the child, passing the primitive value up to the parent (not
always possible)

## Example

```

<script>

 angular.module('inputExample', [])

   .controller('ExampleController', ['$scope', function($scope) {

​     $scope.val = '1';

   }]);

</script>

<style>

  .my-input {

​    transition:all linear 0.5s;

​    background: transparent;

  }

  .my-input.ng-invalid {

​    color:white;

​    background: red;

  }

</style>

<p id="inputDescription">

 Update input to see transitions when valid/invalid.

 Integer is a valid value.

</p>

<form name="testForm" ng-controller="ExampleController">

  <input ng-model="val" ng-pattern="/^\d+$/" name="anim" class="my-input"

​         aria-describedby="inputDescription" />

</form>

```
