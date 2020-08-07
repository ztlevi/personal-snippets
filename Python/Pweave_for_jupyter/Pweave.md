# Pweave

## Installation

```
pip install --upgrade Pweave
```

## Export

```
pweave -f notebook FIR_design.py
```

## Support format

```sh
## Input format
-i INFORMAT, --input-format=INFORMAT
 	Input format: noweb, markdown, notebook or script

## Output format
➜ pweave -l

Pweave supported output formats:

* html:
   HTML with pygments highlighting
* leanpub:
   Leanpub markdown
* markdown:
   Pandoc markdown, same as format pandoc
* md2html:
   Markdown to HTML using Python-Markdown
* notebook:
   Jupyter notebook
* pandoc:
   Pandoc markdown
* pandoc2html:
   Markdown to HTML using Pandoc, requires Pandoc in path
* pandoc2latex:
   Markdown to Latex using Pandoc, requires Pandoc in path
* rst:
   reStructuredText
* softcover:
   SoftCover markdown
* sphinx:
   reStructuredText for Sphinx
* tex:
   Latex with verbatim for code and results
* texminted:
   Latex with predefined minted environment for codeblocks
* texpweave:
   Latex output with user defined formatting using named environments (in latex header)
* texpygments:
   Latex output with pygments highlighted output
```
