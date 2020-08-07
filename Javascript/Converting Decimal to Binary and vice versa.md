Converting Decimal to Binary and vice versa

```js
function dec2bin(dec) {
  return (dec >>> 0).toString(2);
}
```

Converting Binary to Decimal

```js
function bin2dec(bin) {
  return parseInt(bin, 2).toString(10);
}
```
