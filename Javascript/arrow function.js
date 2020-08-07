let counter1;

function setup() {
  console.log(this);
  // window
  noCanvas();
  counter1 = new Counter(100, 500);
}
function draw() {
  counter1.countIt();
}
class Couinter {
  constructor(start, time) {
    this.count = start;
    this.p = createP("");
    console.log(this);
    // Counter
    setInterval(countIt, wait);
    function countIt() {
      console.log(this);
      // window
      this.count++;
      this.p.html(this.count);
    }
  }
}
