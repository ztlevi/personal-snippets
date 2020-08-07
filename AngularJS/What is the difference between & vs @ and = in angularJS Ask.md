@ allows a value defined on the directive attribute to be passed to the directive's isolate scope. The value could be a
simple string value (myattr="hello") or it could be an AngularJS interpolated string with embedded expressions
(myattr="my\_{{helloText}}"). Think of it as "one-way" communication from the parent scope into the child directive.
John Lindquist has a series of short screencasts explaining each of these. Screencast on @ is here:
https://egghead.io/lessons/angularjs-isolate-scope-attribute-binding

& allows the directive's isolate scope to pass values into the parent scope for evaluation in the expression defined in
the attribute. Note that the directive attribute is implicitly an expression and does not use double curly brace
expression syntax. This one is tougher to explain in text. Screencast on & is here:
https://egghead.io/lessons/angularjs-isolate-scope-expression-binding

= sets up a two-way binding expression between the directive's isolate scope and the parent scope. Changes in the child
scope are propagated to the parent and vice-versa. Think of = as a combination of @ and &. Screencast on = is here:
https://egghead.io/lessons/angularjs-isolate-scope-two-way-binding

And finally here is a screencast that shows all three used together in a single view:
https://egghead.io/lessons/angularjs-isolate-scope-review
