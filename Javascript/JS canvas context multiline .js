var c = document.getElementById("c").getContext("2d");
c.font = "11px Courier";
console.log(c);
var txt = "line 1\nline 2\nthird line..";
var x = 30;
var y = 30;
var lineheight = 15;
var lines = txt.split("\n");

for (var i = 0; i < lines.length; i++) c.fillText(lines[i], x, y + i * lineheight);
