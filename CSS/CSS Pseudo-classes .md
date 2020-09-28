## https://www.w3schools.com/css/css_pseudo_classes.asp

# CSS - The :first-child Pseudo-class

The :first-child pseudo-class matches a specified element that is the first child of another
element.

## Match the first <p> element

In the following example, the selector matches any <p> element that is the first child of any
element:

```css
p:first-child {
  color: blue;
}
```

## Match the first <i> element in all <p> elements

In the following example, the selector matches the first <i> element in all <p> elements:

```css
p i:first-child {
  color: blue;
}
```

## Match all <i> elements in all first child <p> elements

In the following example, the selector matches all <i> elements in <p> elements that are the first
child of another element:

```css
p:first-child i {
  color: blue;
}
```

# CSS - The :lang Pseudo-class

The :lang pseudo-class allows you to define special rules for different languages.

In the example below, :lang defines the quotation marks for <q> elements with lang="no":

```html
<html>
  <head>
    <style>
      q:lang(no) {
        quotes: "~""~";
      }
    </style>
  </head>

  <body>
    <p>Some text <q lang="no">A quote in a paragraph</q> Some text.</p>
  </body>
</html>
```
