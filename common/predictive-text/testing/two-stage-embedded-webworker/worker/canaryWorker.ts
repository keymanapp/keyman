var counter = 0;

onmessage = function(e) {
  counter++;
  
  console.log("Message received from main page: ", e.data);
  
  postMessage(counter);
}