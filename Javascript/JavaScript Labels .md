JavaScript Labels To label JavaScript statements you precede the statements with a label name and a colon:

```js
label: statements;
```

The break and the continue statements are the only JavaScript statements that can "jump out of" a code block.

Syntax:

```js
break labelname;

continue labelname;
```

The continue statement (with or without a label reference) can only be used to skip one loop iteration.

The break statement, without a label reference, can only be used to jump out of a loop or a switch.

With a label reference, the break statement can be used to jump out of any code block:

```js
var cars = ["BMW", "Volvo", "Saab", "Ford"];
list: {
  text += cars[0] + "<br>";
  text += cars[1] + "<br>";
  text += cars[2] + "<br>";
  break list;
  text += cars[3] + "<br>";
  text += cars[4] + "<br>";
  text += cars[5] + "<br>";
}
```
