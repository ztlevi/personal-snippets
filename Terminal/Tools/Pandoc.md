# Pandoc Commands

- markdown to org

```
pandoc -f markdown -t org -o newfile.org original-file.markdown
pandoc -f markdown -t asciidoctor -o README.asdoc README.md
```

- Web page to markdown

```
pandoc -s -r html http://www.gnu.org/software/make/ -o example12.md
```
