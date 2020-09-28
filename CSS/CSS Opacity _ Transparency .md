## Transparent Image

The opacity property can take a value from 0.0 - 1.0. The lower value, the more transparent:

> Note: IE8 and earlier use filter:alpha(opacity=x). The x can take a value from 0 - 100. A lower
> value makes the element more transparent.

```css
img {
  opacity: 0.5;
  filter: alpha(opacity=50); /* For IE8 and earlier */
}
```
