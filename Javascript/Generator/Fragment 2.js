const fetch = require("node-fetch");
const co = require("co");

// fetch('http://jsonplaceholder.typicode.com/posts/1')
//   .then(response => response.json())
//   .then(post => post.title)
//   .then(x => console.log('Title:', x))

run(function*() {
  const uri = "http://jsonplaceholder.typicode.com/posts/1";
  const response = yield fetch(uri);
  const post = yield response.json();
  console.log("post is", post);

  const title = post.title;
  console.log("Title:", title);
});

function run(generator) {
  console.log(generator);

  const iterator = generator();
  const iteration = iterator.next();
  const promise = iteration.value;

  promise.then(response => {
    // pass the response to this `yield response.json()`
    const anotherIterator = iterator.next(response);
    const anotherPromose = anotherIterator.value;
    anotherPromose.then(y => iterator.next(y));

    console.log("anotherPromose", anotherPromose);
  });
}
