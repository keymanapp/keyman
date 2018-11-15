// Useful for passing class constructors
type Workable<T> = {
  new (...args: any[]): T;
  // Requires a static implementation.
  onmessage(e: any): void;
};

class A {
  a(x: number, y: number):number {
    return x + y;
  }
}

var WorkerGlobals = {
  counter: 0,
  a: A
}

class WorkerCore {
  static WorkerGlobals = WorkerGlobals;
  //static counter: number;
  
  static onmessage(e: any) {
    WorkerGlobals.counter++;
  
    console.log("Message received from main page: ", e.data);
    
    // Forces TypeScript to interpret this line as plain JavaScript, as it uses a non-Worker definition.
    // @ts-ignore
    postMessage(WorkerGlobals.counter);
  }
}

function createWorkerFromClasses(globals: object, fn: Workable<object>): Worker {
  var sep = ";\n";
  let glb = "var WorkerGlobals = " + JSON.stringify(globals);
  let wc = "var onmessage = " + fn.onmessage.toString();
  var blob = new Blob([glb, sep, wc], { type: 'text/javascript' });
  let url = URL.createObjectURL(blob);

  return new Worker(url);
}

var canaryWorker = createWorkerFromClasses(WorkerGlobals, WorkerCore);

canaryWorker.onmessage = function(e) {
  var counter = e.data; // Number of times the WebWorker has been messaged.
  console.log("Received message from the WebWorker: " + e.data);
  
  var txtFeedback = <HTMLInputElement>document.getElementById("txtFeedback");
  txtFeedback.value = counter + " click(s)";
}
