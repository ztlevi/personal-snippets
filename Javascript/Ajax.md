# GET

```js
var xhttp = new XMLHttpRequest();
xhttp.onreadystatechange = function() {
  if (this.readyState == 4 && this.status == 200) {
    document.getElementById("demo").innerHTML = this.responseText;
  }
};
xhttp.open("GET", "demo_get.asp?t=" + Math.random(), true);
xhttp.send();
```

# POST

```js
var xhttp = new XMLHttpRequest();
xhttp.onreadystatechange = function() {
  if (this.readyState == 4 && this.status == 200) {
    document.getElementById("demo").innerHTML = this.responseText;
  }
};
xhttp.open("POST", "demo_post2.asp", true);
xhttp.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
xhttp.send("fname=Henry&lname=Ford");
```

# AJAX - Server Response

The onreadystatechange Property The readyState property holds the status of the XMLHttpRequest.

The onreadystatechange property defines a function to be executed when the readyState changes.

The status property and the statusText property holds the status of the XMLHttpRequest object.

| Property           | Description                                                                                                                                                                                                      |
| ------------------ | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| onreadystatechange | Defines a function to be called when the readyState property changes                                                                                                                                             |
| readyState         | Holds the status of the XMLHttpRequest. <br> 0: request not initialized <br> 1: server connection established <br> 2: request received <br> 3: processing request <br> 4: request finished and response is ready |
| status             | 200: "OK" <br> 403: "Forbidden" <br> 404: "Page not found" <br> For a complete list go to the Http Messages Reference                                                                                            |
| statusText         | Returns the status-text (e.g. "OK" or "Not Found")                                                                                                                                                               |
