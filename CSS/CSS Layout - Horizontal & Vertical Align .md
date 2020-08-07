## https://www.w3schools.com/css/css_align.asp

# Center Align Elements

To horizontally center a block element (like <div>), use `margin: auto;`

```css
.center {
  margin: auto;
  width: 50%;
  border: 3px solid green;
  padding: 10px;
}
```

# Center Align Text

To just center the text inside an element, use `text-align: center;`

# Center Vertically - Using padding

There are many ways to center an element vertically in CSS. A simple solution is to use top and bottom padding:

```css
.center {
  padding: 70px 0;
  border: 3px solid green;
}
```

# Center Vertically - Using line-height

Another trick is to use the line-height property with a value that is equal to the height property.

```css
.center {
  line-height: 200px;
  height: 200px;
  border: 3px solid green;
  text-align: center;
}

/* If the text has multiple lines, add the following: */
.center p {
  line-height: 1.5;
  display: inline-block;
  vertical-align: middle;
}
```

# Center Vertically - Using position & transform

If padding and line-height is not an option, a third solution is to use positioning and the transform property:

```css
.center {
  height: 200px;
  position: relative;
  border: 3px solid green;
}

.center p {
  margin: 0;
  position: absolute;
  top: 50%;
  left: 50%;
  transform: translate(-50%, -50%);
}
```
