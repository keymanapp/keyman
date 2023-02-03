function addInputs()
{
  var masterDiv = document.getElementById('DynamicTextboxes');

  var newTextArea = document.createElement("textarea");
  newTextArea.id = 'ta2';
  newTextArea.className = 'test';
  newTextArea.placeholder = "Dynamic area!";

  var newInput = document.createElement("input");
  newInput.id = 'in2';
  newInput.className = 'test';
  newInput.placeholder = "Dynamic area!";

  masterDiv.appendChild(newTextArea);
  masterDiv.appendChild(newInput);
}