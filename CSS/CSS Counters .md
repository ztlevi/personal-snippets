## https://www.w3schools.com/css/css_counters.asp

# Automatic Numbering With Counters

CSS counters are like "variables". The variable values can be incremented by CSS rules (which will
track how many times they are used).

To work with CSS counters we will use the following properties:

- counter-reset - Creates or resets a counter
- counter-increment - Increments a counter value
- content - Inserts generated content
- counter() or counters() function - Adds the value of a counter to an element To use a CSS counter,
  it must first be created with counter-reset.

The following example creates a counter for the page (in the body selector), then increments the
counter value for each

<h2> element and adds "Section <value of the counter>:" to the beginning of each <h2> element:

```css
body {
  counter-reset: section;
}

h2::before {
  counter-increment: section;
  content: "Section " counter(section) ": ";
}
```

## Nesting Counters

The following example creates one counter for the page (section) and one counter for each <h1>
element (subsection). The "section" counter will be counted for each <h1> element with "Section
<value of the section counter>.", and the "subsection" counter will be counted for each <h2> element
with "<value of the section counter>.<value of the subsection counter>":

```css
body {
  counter-reset: section;
}

h1 {
  counter-reset: subsection;
}

h1::before {
  counter-increment: section;
  content: "Section " counter(section) ". ";
}

h2::before {
  counter-increment: subsection;
  content: counter(section) "." counter(subsection) " ";
}
```
