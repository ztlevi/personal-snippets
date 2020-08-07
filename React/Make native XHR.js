function makeRequest(method, url, done) {
  var xhr = new XMLHttpRequest();
  xhr.open(method, url);
  xhr.onload = function() {
    done(null, xhr.response);
  };
  xhr.onerror = function() {
    done(xhr.response);
  };
  xhr.send();
}

// And we'd call it as such:

makeRequest("GET", "http://example.com", function(err, datums) {
  if (err) {
    throw err;
  }
  console.log(datums);
});
