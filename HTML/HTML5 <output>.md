HTML5 <output> Element The <output> element represents the result of a calculation (like one
performed by a script).

```html
<form action="/action_page.php" oninput="x.value=parseInt(a.value)+parseInt(b.value)">
  0
  <input type="range" id="a" name="a" value="50" />
  100 +
  <input type="number" id="b" name="b" value="50" />
  =
  <output name="x" for="a b"></output>
  <br /><br />
  <input type="submit" />
</form>
```
