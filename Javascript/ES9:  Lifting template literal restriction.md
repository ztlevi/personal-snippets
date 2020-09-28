And one for the next year in ES9 — Lifting template literal restriction With the tagged template
literal (ES6) we can do stuff like declaring a template parsing function and returning a value
according to our logic:

```
const esth = 8;
helper`ES ${esth} is `;
function helper(strs, ...keys) {
  const str1 = strs[0]; // ES
  const str2 = strs[1]; // is
  let additionalPart = '';
  if (keys[0] == 8) { // 8
    additionalPart = 'awesome';
  }
  else {
    additionalPart = 'good';
  }

  return `${str1} ${keys[0]} ${str2} ${additionalPart}.`;
}
```

The returned value will be → ES 8 is awesome. And for esth of 7 the returned value will be → ES 7 is
good.
