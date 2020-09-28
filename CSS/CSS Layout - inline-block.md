## https://www.w3schools.com/css/css_inline-block.asp

CSS Layout - inline-block

```css
.floating-box {
  float: left;
  width: 150px;
  height: 75px;
  margin: 10px;
  border: 3px solid #73ad21;
}

.after-box {
  clear: left;
}
```

The same effect can be achieved by using the inline-block value of the display property (notice that
no clear property is needed):

```css
.floating-box {
  display: inline-block;
  width: 150px;
  height: 75px;
  margin: 10px;
  border: 3px solid #73ad21;
}
```
