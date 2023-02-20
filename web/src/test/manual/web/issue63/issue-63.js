{
var inputCounter = 1;
var removalCounter = 1;
var removedElements = [];
}

function addInputs() {
  var masterDiv = document.getElementById('DynamicTextboxes');

  var newTextArea = document.createElement("textarea");

  var i = inputCounter++;

  newTextArea.id = 'ta' + i;
  newTextArea.className = 'test';
  newTextArea.placeholder = "Dynamic area #" + i + "!";

  var newInput = document.createElement("input");
  newInput.id = 'in' + i;
  newInput.className = 'test';
  newInput.placeholder = "Dynamic area #" + i + "!";

  //masterDiv.appendChild(newTextArea);
  //masterDiv.appendChild(newInput);

  var newDiv = document.createElement("div");
  newDiv.id = "dynamic_div" + i;

  //newDiv.appendChild(newTextArea);
  newDiv.appendChild(newInput);

  masterDiv.appendChild(newDiv);
}

function removeInputs() {
  var parent = document.getElementById('DynamicTextboxes');
  var div = document.getElementById("dynamic_div" + removalCounter);
  if(div) {
	removalCounter++;
	parent.removeChild(div);
	removedElements.push(div);
  }
}

function restoreInputs() {
  var masterDiv = document.getElementById('DynamicTextboxes');
  removalCounter = 1;

  for(var i = 0; i < removedElements.length; i++)
  {
	  masterDiv.appendChild(removedElements[i]);
  }

  removedElements = [];
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