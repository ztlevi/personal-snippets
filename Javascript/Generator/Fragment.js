const fetch = require("node-fetch");

// fetch('http://jsonplaceholder.typicode.com/posts/1')
//   .then(response => response.json())
//   .then(post => post.title)
//   .then(x => console.log('Title:', x))

run(function* () {
  const uri = "http://jsonplaceholder.typicode.com/posts/1";
  const response = yield fetch(uri);
  const post = yield response.json();
  const title = post.title;
  return title;
})
  .then((x) => console.log("run result in", x))
  .catch((error) => console.error(error.stack));

function run(generator) {
  const iterator = generator();

  function iterate(iteration) {
    if (iteration.done) {
      return iteration.value;
    }
    const promise = iteration.value;
    return promise.then((x) => iterate(iterator.next(x)));
  }
  return iterate(iterator.next());
}
