## https://www.w3schools.com/css/css3_box-sizing.asp

# CSS Box Sizing

The CSS box-sizing property allows us to include the padding and border in an element's total width
and height.

## Without the CSS box-sizing Property

By default, the width and height of an element is calculated like this:

width + padding + border = actual width of an element height + padding + border = actual height of
an element

## With the CSS box-sizing Property

The box-sizing property allows us to include the padding and border in an element's total width and
height.

If you set box-sizing: border-box; on an element padding and border are included in the width and
height:

### Universal Box Sizing

```css
*,
*:before,
*:after {
  box-sizing: border-box;
}
```

### Universal Box Sizing with Inheritance

```css
html {
  box-sizing: border-box;
}
*,
*:before,
*:after {
  box-sizing: inherit;
}
```

### Vendor Prefixes

Every current browser supports box-sizing: border-box; unprefixed, so the need for vendor prefixes
is fading. But, if you need to support older versions of Safari (< 5.1), Chrome (< 10), and Firefox
(< 29), you should include the prefixes -webkit and -moz, like this:

```css
html {
  -webkit-box-sizing: border-box;
  -moz-box-sizing: border-box;
  box-sizing: border-box;
}
*,
*:before,
*:after {
  -webkit-box-sizing: inherit;
  -moz-box-sizing: inherit;
  box-sizing: inherit;
}
```
