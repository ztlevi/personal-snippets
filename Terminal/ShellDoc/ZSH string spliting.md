# String splitting

```sh
st="aaa.bbb.ccc"
a=(${(s:.:)st})
declare -p a
echo ${#a}

>> typeset -a a
>> a=( aaa bbb ccc )
>> 3
```
