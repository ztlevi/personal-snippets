## https://developers.google.com/web/fundamentals/performance/rendering/optimize-javascript-execution

# Use requestAnimationFrame for visual changes

When visual changes are happening on screen you want to do your work at the right time for the browser, which is right
at the start of the frame. The only way to guarantee that your JavaScript will run at the start of a frame is to use
requestAnimationFrame.

```js
/**
 * If run as a requestAnimationFrame callback, this
 * will be run at the start of the frame.
 */
function updateScreen(time) {
  // Make visual updates here.
}

requestAnimationFrame(updateScreen);
```

Frameworks or samples may use setTimeout or setInterval to do visual changes like animations, but the problem with this
is that the callback will run at some point in the frame, possibly right at the end, and that can often have the effect
of causing us to miss a frame, resulting in jank.

# Reduce complexity or use Web Workers

JavaScript runs on the browser’s main thread, right alongside style calculations, layout, and, in many cases, paint. If
your JavaScript runs for a long time, it will block these other tasks, potentially causing frames to be missed.

You should be tactical about when JavaScript runs, and for how long. For example, if you’re in an animation like
scrolling, you should ideally be looking to keep your JavaScript to something in the region of 3-4ms. Any longer than
that and you risk taking up too much time. If you’re in an idle period, you can afford to be more relaxed about the time
taken.

In many cases you can move pure computational work to Web Workers, if, for example, it doesn’t require DOM access. Data
manipulation or traversal, like sorting or searching, are often good fits for this model, as are loading and model
generation.

```js
var dataSortWorker = new Worker("sort-worker.js");
dataSortWorker.postMesssage(dataToSort);

// The main thread is now free to continue working on other things...

dataSortWorker.addEventListener("message", function(evt) {
  var sortedData = evt.data;
  // Update data on screen...
});
```

Not all work can fit this model: Web Workers do not have DOM access. Where your work must be on the main thread,
consider a batching approach, where you segment the larger task into micro-tasks, each taking no longer than a few
milliseconds, and run inside of requestAnimationFrame handlers across each frame.

```js
var taskList = breakBigTaskIntoMicroTasks(monsterTaskList);
requestAnimationFrame(processTaskList);

function processTaskList(taskStartTime) {
  var taskFinishTime;

  do {
    // Assume the next task is pushed onto a stack.
    var nextTask = taskList.pop();

    // Process nextTask.
    processTask(nextTask);

    // Go again if there’s enough time to do the next task.
    taskFinishTime = window.performance.now();
  } while (taskFinishTime - taskStartTime < 3);

  if (taskList.length > 0) requestAnimationFrame(processTaskList);
}
```

There are UX and UI consequences to this approach, and you will need to ensure that the user knows that a task is being
processed, either by using a progress or activity indicator. In any case this approach will keep your app's main thread
free, helping it to stay responsive to user interactions.

# Avoid micro-optimizing your JavaScript

It may be cool to know that the browser can execute one version of a thing 100 times faster than another thing, like
that requesting and element’s `offsetTop` is faster than computing `getBoundingClientRect()`, but it’s almost always
true that you’ll only be calling functions like these a small number of times per frame, so it’s normally wasted effort
to focus on this aspect of JavaScript’s performance. You'll typically only save fractions of milliseconds.

If you’re making a game, or a computationally expensive application, then you’re likely an exception to this guidance,
as you’ll be typically fitting a lot of computation into a single frame, and in that case everything helps.

In short, you should be very wary of micro-optimizations because they won’t typically map to the kind of application
you’re building.
