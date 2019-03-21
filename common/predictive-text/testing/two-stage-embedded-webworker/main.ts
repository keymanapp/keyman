// Provides the final source for our compiled WebWorker within a wrapping function.
/// <reference path="embedded_worker.js" />

function createWorker(fn: Function): Worker {
  var str_fn = fn.toString();

  // We now unwrap our WebWorker from its function.
  var str_lines = str_fn.split("\n");
  var blob_lines = []

  for(var i=1; i < str_lines.length -1; i++) {
    blob_lines.push(str_lines[i] + "\n");
  }
  // Unwrapping complete.

  var blob = new Blob(blob_lines, { type: 'text/javascript' });
  let url = URL.createObjectURL(blob);

  return new Worker(url);
}

var canaryWorker = createWorker(LMLayerWorker);

canaryWorker.onmessage = function(e) {
  var counter = e.data; // Number of times the WebWorker has been messaged.
  console.log("Received message from the WebWorker: " + e.data);
  
  var txtFeedback = <HTMLInputElement>document.getElementById("txtFeedback");
  txtFeedback.value = counter + " click(s)";
}
