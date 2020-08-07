Here's a snazzier Ecmascript 6 version:

input: Array[Array]

```
const zip= rows=>rows[0].map((_,c)=>rows.map(row=>row[c]))
```
