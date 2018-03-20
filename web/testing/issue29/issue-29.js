{
var inputCounter = 1;
}

function addInputs() {
  var masterDiv = document.getElementById('DynamicTextboxes');

  var newTextArea = document.createElement("textarea");

  var i = inputCounter++;

  newTextArea.id = 'ta' + i;
  newTextArea.className = 'test';
  newTextArea.placeholder = "Dynamic area!";

  var newInput = document.createElement("input");
  newInput.id = 'in' + i;
  newInput.className = 'test';
  newInput.placeholder = "Dynamic area!";

  //masterDiv.appendChild(newTextArea);
  //masterDiv.appendChild(newInput);

  var newDiv = document.createElement("div");

  newDiv.appendChild(newTextArea);
  newDiv.appendChild(newInput);

  masterDiv.appendChild(newDiv);
}

function addIFrame() {
  var masterDiv = document.getElementById('DynamicTextboxes');

  var frame = document.createElement("iframe");
  frame.height = "100";
  frame.src = "issue-29-iframe.html";
  frame.onload = function() {console.log('Original onload!');};

  //masterDiv.appendChild(frame);

  var newDiv = document.createElement("div");
  newDiv.appendChild(frame);
  masterDiv.appendChild(newDiv);
}