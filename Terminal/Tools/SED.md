# SED

At its core sed is a stream editor. It excels at substitutions, but can also be leveraged for all out refactoring.

The most basic sed command consists of `s/old/new/g`. This translates to search for old value, replace with new
globally. Without the `/g` our command would terminate after the first occurrence.

To get a quick taste of the power lets dive into an example. In this scenario you’ve been given the following file:

```
balance,name
$1,000,john
$2,000,jack
```

The first thing we may want to do is remove the dollar signs. The `-i` flag indicates in-place. The `''` is to indicate
a zero-length file extension, thus overwriting our initial file. Ideally, you would test each of these individually and
then output to a new file.

```
sed -i '' 's/\$//g' data.txt

# balance,name
# 1,000,john
# 2,000,jack
```

Next up, the commas in our balance column values.

```
sed -i '' 's/\([0-9]\),\([0-9]\)/\1\2/g' data.txt

# balance,name
# 1000,john
# 2000,jack
```

Lastly, Jack up and decided to quit one day. So, au revoir, mon ami.

```
sed -i '' '/jack/d' data.txt

# balance,name
# 1000,john
```

As you can see, sed packs quite a punch, but the fun doesn’t stop there.
