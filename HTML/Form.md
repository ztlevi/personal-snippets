## https://www.w3schools.com/html/html_forms.asp

# The Target Attribute

The target attribute specifies if the submitted result will open in a new browser tab, a frame, or
in the current window.

The default value is "\_self" which means the form will be submitted in the current window.

To make the form result open in a new browser tab, use the value "\_blank":

`<form action="/action_page.php" target="_blank">`

# The Method Attribute

The method attribute specifies the HTTP method (GET or POST) to be used when submitting the form
data: `<form action="/action_page.php" method="get">` or
`<form action="/action_page.php" method="post">`

# Grouping Form Data with <fieldset>

The <fieldset> element is used to group related data in a form.

The <legend> element defines a caption for the fieldset> element.

```html
<form action="/action_page.php">
  <fieldset>
    <legend>Personal information:</legend>
    First name:<br />
    <input type="text" name="firstname" value="Mickey" /><br />
    Last name:<br />
    <input type="text" name="lastname" value="Mouse" /><br /><br />
    <input type="submit" value="Submit" />
  </fieldset>
</form>
```
