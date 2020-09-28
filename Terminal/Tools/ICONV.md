# ICONV

File encodings can be tricky. For the most part files these days are all UTF-8 encoded. To
understand some of the magic behind UTF-8, check out this excellent video. Nonetheless, there are
times where we receive a file that isn’t in this format. This can lead to some wonky attempts at
swapping the encoding schema. Here, iconv is a life saver. Iconv is a simple program that will take
text in one encoding and output the text in another.

```shell
# Converting -f (from) latin1 (ISO-8859-1)
# -t (to) standard UTF_8

iconv -f ISO-8859-1 -t UTF-8 < input.txt > output.txt
```

- Useful options:

  - iconv -l list all known encodings
  - iconv -c silently discard characters that cannot be converted
