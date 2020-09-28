Then we can add `overflow: auto;` to the containing element to fix this problem:

```css
.clearfix {
  overflow: auto;
}
```

The `overflow:auto` clearfix works well as long as you are able to keep control of your margins and
padding (else you might see scrollbars). The new, modern clearfix hack however, is safer to use, and
the following code is used for most webpages:

```css
.clearfix::after {
  content: "";
  clear: both;
  display: table;
}
```
