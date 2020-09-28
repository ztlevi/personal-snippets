# Navigating Between Nodes

You can use the following node properties to navigate between nodes with JavaScript:

- parentNode
- childNodes[nodenumber]
- firstChild
- lastChild
- nextSibling
- previousSibling

```js
<html>
  <body>
    <h1 id="id01">My First Page</h1>
    <p id="id02">Hello!</p>

    <script>
      document.getElementById("id02").innerHTML =
      document.getElementById("id01").childNodes[0].nodeValue;
    </script>
  </body>
</html>
```

# The nodeName Property

The nodeName property specifies the name of a node.

- nodeName is read-only
- nodeName of an element node is the same as the tag name
- nodeName of an attribute node is the attribute name
- nodeName of a text node is always #text
- nodeName of the document node is always #document

# The nodeValue Property

The nodeValue property specifies the value of a node.

- nodeValue for element nodes is undefined
- nodeValue for text nodes is the text itself
- nodeValue for attribute nodes is the attribute value

# The nodeType Property

The nodeType property is read only. It returns the type of a node.

| Node               | Type | Example                                         |
| ------------------ | ---- | ----------------------------------------------- |
| ELEMENT_NODE       | 1    | `<h1 class="heading">W3Schools</h1>`            |
| ATTRIBUTE_NODE     | 2    | class = "heading" (deprecated)                  |
| TEXT_NODE          | 3    | W3Schools                                       |
| COMMENT_NODE       | 8    | `<!-- This is a comment -->`                    |
| DOCUMENT_NODE      | 9    | The HTML document itself (the parent of <html>) |
| DOCUMENT_TYPE_NODE | 10   | `<!Doctype html>`                               |
