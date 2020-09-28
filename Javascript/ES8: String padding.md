This section adds two functions to the String object: padStart & padEnd. As their names, the purpose
of those functions is to pad the start or the end of the string, so that the resulting string
reaches the given length. You can pad it with specific character or string or just pad with spaces
by default. Here are the functions declarations:

```
str.padStart(targetLength [, padString])
str.padEnd(targetLength [, padString])
```

```
'es8'.padStart(2);          // 'es8'
'es8'.padStart(5);          // '  es8'
'es8'.padStart(6, 'woof');  // 'wooes8'
'es8'.padStart(14, 'wow');  // 'wowwowwowwoes8'
'es8'.padStart(7, '0');     // '0000es8'
'es8’.padEnd(2);          // 'es8'
'es8’.padEnd(5);          // 'es8  '
'es8’.padEnd(6, 'woof’);  // 'es8woo'
'es8’.padEnd(14, 'wow’);  // 'es8wowwowwowwo'
'es8’.padEnd(7, '6’);     // 'es86666'
```
