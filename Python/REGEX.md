# Python Regular Expressions

https://developers.google.com/edu/python/regular-expressions

Regular expressions are a powerful language for matching text patterns. This page gives a basic introduction to regular
expressions themselves sufficient for our Python exercises and shows how regular expressions work in Python. The Python
"re" module provides regular expression support.

<!-- link to better/more regex materials here, e.g. http://www.amk.ca/python/howto/regex/ -->

In Python a regular expression search is typically written as:

```python
  match = re.search(pat, str)
```

The re.search() method takes a regular expression pattern and a string and searches for that pattern within the string.
If the search is successful, search() returns a match object or None otherwise. Therefore, the search is usually
immediately followed by an if-statement to test if the search succeeded, as shown in the following example which
searches for the pattern 'word:' followed by a 3 letter word (details below):

```python
str = 'an example word:cat!!'
match = re.search(r'word:\w\w\w', str)  # If-statement after search() tests if it succeeded  if match:
  print 'found', match.group() ## 'found word:cat'  else:
  print 'did not find'
```

The code `match = re.search(pat, str)` stores the search result in a variable named "match". Then the if-statement tests
the match -- if true the search succeeded and match.group() is the matching text (e.g. 'word:cat'). Otherwise if the
match is false (None to be more specific), then the search did not succeed, and there is no matching text.

The 'r' at the start of the pattern string designates a python "raw" string which passes through backslashes without
change which is very handy for regular expressions (Java needs this feature badly!). I recommend that you always write
pattern strings with the 'r' just as a habit.

## [](https://developers.google.com/edu/python/regular-expressions#top_of_page)Basic Patterns

The power of regular expressions is that they can specify patterns, not just fixed characters. Here are the most basic
patterns which match single chars:

- a, X, 9, < -- ordinary characters just match themselves exactly. The meta-characters which do not match themselves
  because they have special meanings are: . ^ \$ \* + ? { [ ] \ | ( ) (details below)

* . (a period) -- matches any single character except newline '\n'

- \w -- (lowercase w) matches a "word" character: a letter or digit or underbar [a-zA-Z0-9_]. Note that although "word"
  is the mnemonic for this, it only matches a single word char, not a whole word. \W (upper case W) matches any non-word
  character.

* \b -- boundary between word and non-word

- \s -- (lowercase s) matches a single whitespace character -- space, newline, return, tab, form [ \n\r\t\f]. \S (upper
  case S) matches any non-whitespace character.

* \t, \n, \r -- tab, newline, return

- \d -- decimal digit [0-9] (some older regex utilities do not support but \d, but they all support \w and \s)

* ^ = start, \$ = end -- match the start or end of the string

- \ -- inhibit the "specialness" of a character. So, for example, use \. to match a period or \\ to match a slash. If
  you are unsure if a character has special meaning, such as '@', you can put a slash in front of it, \@, to make sure
  it is treated just as a character.

## Compare HTML tags[¶](https://www.pythonsheets.com/notes/python-rexp.html#compare-html-tags "Permalink to this headline")

| tag type   | format        | example      |
| ---------- | ------------- | ------------ |
| all tag    | <[^>]+>       | <br />, <a>  |
| open tag   | <[^/>][^>]\*> | <a>, <table> |
| close tag  | </[^>]+>      | </p>, </a>   |
| self close | <[^/>]+/>     | <br />       |

```python
# open tag
>>> re.search('<[^/>][^>]*>', '<table>') != None
True
>>> re.search('<[^/>][^>]*>', '<a href="#label">') != None
True
>>> re.search('<[^/>][^>]*>', '<img src="/img">') != None
True
>>> re.search('<[^/>][^>]*>', '</table>') != None
False

# close tag
>>> re.search('</[^>]+>', '</table>') != None
True

# self close
>>> re.search('<[^/>]+/>', '<br />') != None
True
```

## `re.findall()` match string[¶](https://www.pythonsheets.com/notes/python-rexp.html#re-findall-match-string "Permalink to this headline")

```python
# split all string
>>>source = "Hello World Ker HAHA"
>>>re.findall('[\w]+', source)
['Hello', 'World', 'Ker', 'HAHA']

# parsing python.org website
>>> import urllib
>>> import re
>>> s = urllib.urlopen('https://www.python.org')
>>> html = s.read()
>>> s.close()
>>> print("open tags")
open tags
>>> re.findall('<[^/>][^>]*>', html)[0:2]
['<!doctype html>', '<!--[if lt IE 7]>']
>>> print("close tags")
close tags
>>> re.findall('</[^>]+>', html)[0:2]
['</script>', '</title>']
>>> print("self-closing tags")
self-closing tags
>>> re.findall('<[^/>]+/>', html)[0:2]
[]
```

## Group Comparison[¶](https://www.pythonsheets.com/notes/python-rexp.html#group-comparison "Permalink to this headline")

```python
# (...) group a regular expression
>>> m = re.search(r'(\d{4})-(\d{2})-(\d{2})', '2016-01-01')
>>> m
<_sre.SRE_Match object; span=(0, 10), match='2016-01-01'>
>>> m.groups()
('2016', '01', '01')
>>> m.group()
'2016-01-01'
>>> m.group(1)
'2016'
>>> m.group(2)
'01'
>>> m.group(3)
'01'

# Nesting groups
>>> m = re.search(r'(((\d{4})-\d{2})-\d{2})', '2016-01-01')
>>> m.groups()
('2016-01-01', '2016-01', '2016')
>>> m.group()
'2016-01-01'
>>> m.group(1)
'2016-01-01'
>>> m.group(2)
'2016-01'
>>> m.group(3)
'2016'
```

## Non capturing group[¶](https://www.pythonsheets.com/notes/python-rexp.html#non-capturing-group "Permalink to this headline")

```python
# non capturing group
>>>url = 'http://stackoverflow.com/'
>>> m = re.search('(?:http|ftp)://([^/\r\n]+)(/[^\r\n]*)?', url)
>>> m.groups()
('stackoverflow.com', '/')

# capturing group
>>> m = re.search('(http|ftp)://([^/\r\n]+)(/[^\r\n]*)?', url)
>>> m.groups()
('http', 'stackoverflow.com', '/')
```

## Back Reference[¶](https://www.pythonsheets.com/notes/python-rexp.html#back-reference "Permalink to this headline")

```python
# compare 'aa', 'bb'
>>> re.search(r'([a-z])\1$','aa') != None
True
>>> re.search(r'([a-z])\1$','bb') != None
True
>>> re.search(r'([a-z])\1$','ab') != None
False

# compare open tag and close tag
>>> pattern = r'<([^>]+)>[\s\S]*?</\1>'
>>> re.search(pattern, '<bold> test </bold>') != None
True
>>> re.search(pattern, '<h1> test </h1>') != None
True
>>> re.search(pattern, '<bold> test </h1>') != None
False
```

## Named Grouping `(?P<name>)`[¶](https://www.pythonsheets.com/notes/python-rexp.html#named-grouping-p-name "Permalink to this headline")

```python
# group reference ``(?P<name>...)``
>>>pattern = '(?P<year>\d{4})-(?P<month>\d{2})-(?P<day>\d{2})'
>>> m = re.search(pattern, '2016-01-01')
>>> m.group('year')
'2016'
>>> m.group('month')
'01'
>>> m.group('day')
'01'

# back reference ``(?P=name)``
>>> re.search('^(?P<char>[a-z])(?P=char)','aa')
<_sre.SRE_Match object at 0x10ae0f288>
```

## Substitute String[¶](https://www.pythonsheets.com/notes/python-rexp.html#substitute-string "Permalink to this headline")

```python
# basic substitute
>>> res= "1a2b3c"
>>> re.sub(r'[a-z]',' ', res)
'1 2 3 '

# substitute with group reference
>>> date = r'2016-01-01'
>>> re.sub(r'(\d{4})-(\d{2})-(\d{2})',r'\2/\3/\1/',date)
'01/01/2016/'

# camelcase to underscore
>>> def convert(s):
...     res = re.sub(r'(.)([A-Z][a-z]+)',r'\1_\2', s)
...     return re.sub(r'([a-z])([A-Z])',r'\1_\2', res).lower()
...
>>> convert('CamelCase')
'camel_case'
>>> convert('CamelCamelCase')
'camel_camel_case'
>>> convert('SimpleHTTPServer')
'simple_http_server'
```

## Look around[¶](https://www.pythonsheets.com/notes/python-rexp.html#look-around "Permalink to this headline")

<colgroup>
<col style="width: 42%">
<col style="width: 58%">
</colgroup>
notation   | compare direction
---------- | -----------------
`(?=...)`  | left to right    
`(?!...)`  | left to right    
`(?<=...)` | right to left    
`(?!<...)` | right to left

```python
# basic>>> re.sub('(?=\d{3})', ' ', '12345')
' 1 2 345'
>>> re.sub('(?!\d{3})', ' ', '12345')
'123 4 5 '
>>> re.sub('(?<=\d{3})', ' ', '12345')
'123 4 5 '
>>> re.sub('(?<!\d{3})', ' ', '12345')
' 1 2 345'
```

## Match common username or password[¶](https://www.pythonsheets.com/notes/python-rexp.html#match-common-username-or-password "Permalink to this headline")

```python
>>> re.match('^[a-zA-Z0-9-_]{3,16}$', 'Foo') is not None
True
>>> re.match('^\w|[-_]{3,16}$', 'Foo') is not None
True
```

## Match hex color value[¶](https://www.pythonsheets.com/notes/python-rexp.html#match-hex-color-value "Permalink to this headline")

```python
>>> re.match('^#?([a-f0-9]{6}|[a-f0-9]{3})$', '#ffffff')
<_sre.SRE_Match object at 0x10886f6c0>
>>> re.match('^#?([a-f0-9]{6}|[a-f0-9]{3})$', '#fffffh')
<_sre.SRE_Match object at 0x10886f288>
```

## Match email[¶](https://www.pythonsheets.com/notes/python-rexp.html#match-email "Permalink to this headline")

```python
>>> re.match('^([a-z0-9_\.-]+)@([\da-z\.-]+)\.([a-z\.]{2,6})$',
...          'hello.world@example.com')
<_sre.SRE_Match object at 0x1087a4d40>

# or

>>> exp = re.compile(r'''^([a-zA-Z0-9._%-]+@
...                   [a-zA-Z0-9.-]+
                      \.[a-zA-Z]{2,4})*$''', re.X)
>>> exp.match('hello.world@example.hello.com')
<_sre.SRE_Match object at 0x1083efd50>
>>> exp.match('hello%world@example.hello.com')
<_sre.SRE_Match object at 0x1083efeb8>
```

## Match URL[¶](https://www.pythonsheets.com/notes/python-rexp.html#match-url "Permalink to this headline")

```python
>>> exp = re.compile(r'''^(https?:\/\/)? # match http or https
...             ([\da-z\.-]+)            # match domain
...             \.([a-z\.]{2,6})         # match domain
...             ([\/\w \.-]*)\/?$        # match api or file
...             ''', re.X)
>>>
>>> exp.match('www.google.com')
<_sre.SRE_Match object at 0x10f01ddf8>
>>> exp.match('http://www.example')
<_sre.SRE_Match object at 0x10f01dd50>
>>> exp.match('http://www.example/file.html')
<_sre.SRE_Match object at 0x10f01ddf8>
>>> exp.match('http://www.example/file!.html')
```

## Match IP address[¶](https://www.pythonsheets.com/notes/python-rexp.html#match-ip-address "Permalink to this headline")

<colgroup>
<col style="width: 41%">
<col style="width: 59%">
</colgroup>
col 1          | col 2                
-------------- | ---------------------
notation       | description          
(?:…)          | Don’t capture group  
25[0-5]        | Match 251-255 pattern
2[0-4][0-9]    | Match 200-249 pattern
[1]?[0-9][0-9] | Match 0-199   pattern

```python
>>> exp = re.compile(r'''^(?:(?:25[0-5]
...                      |2[0-4][0-9]
...                      |[1]?[0-9][0-9]?)\.){3}
...                      (?:25[0-5]
...                      |2[0-4][0-9]
...                      |[1]?[0-9][0-9]?)$''', re.X)
>>> exp.match('192.168.1.1')
<_sre.SRE_Match object at 0x108f47ac0>
>>> exp.match('255.255.255.0')
<_sre.SRE_Match object at 0x108f47b28>
>>> exp.match('172.17.0.5')
<_sre.SRE_Match object at 0x108f47ac0>
>>> exp.match('256.0.0.0') is None
True
```

## Match Mac address[¶](https://www.pythonsheets.com/notes/python-rexp.html#match-mac-address "Permalink to this headline")

```python
>>> import random
>>> mac=[random.randint(0x00, 0x7f),
...        random.randint(0x00, 0x7f),
...        random.randint(0x00, 0x7f),
...        random.randint(0x00, 0x7f),
...        random.randint(0x00, 0x7f),
...        random.randint(0x00, 0x7f)]
>>> mac = ':'.join(map(lambda x: "%02x" % x, mac))
>>> mac
'3c:38:51:05:03:1e'
>>> exp = re.compile(r'''[0-9a-f]{2}([:])
...                      [0-9a-f]{2}
...                      (\1[0-9a-f]{2}){4}$''', re.X)
>>> exp.match(mac) is not None
True
```

## Lexer[¶](https://www.pythonsheets.com/notes/python-rexp.html#lexer "Permalink to this headline")

```python
>>> import re
>>> from collectionsimport namedtuple
>>> tokens = [r'(?P<NUMBER>\d+)',
...           r'(?P<PLUS>\+)',
...           r'(?P<MINUS>-)',
...           r'(?P<TIMES>\*)',
...           r'(?P<DIVIDE>/)',
...           r'(?P<WS>\s+)']
>>> lex = re.compile('|'.join(tokens))
>>> Token = namedtuple('Token', ['type', 'value'])
>>> def tokenize(text):
...     scan = lex.scanner(text)
...     return (Token(m.lastgroup, m.group())
...         for m in iter(scan.match, None) if m.lastgroup != 'WS')
...
>>> for _t in tokenize('9 + 5 * 2 - 7'):
...     print(_t)
...
Token(type='NUMBER', value='9')
Token(type='PLUS', value='+')
Token(type='NUMBER', value='5')
Token(type='TIMES', value='*')
Token(type='NUMBER', value='2')
Token(type='MINUS', value='-')
Token(type='NUMBER', value='7')
```
