// Defines an object for dynamically adding elements for testing purposes.
// Designed for use with the robustAttachment.html fixture.

export class DynamicElements {
  static inputCounter = 0;

  static addInput() {
    var masterDiv = document.getElementById('DynamicElements');
    var newInput = document.createElement("input");
    var i = this.inputCounter++;

    newInput.id = 'input' + i;
    newInput.className = 'test';
    newInput.placeholder = "Dynamic area #" + i + "!";

    masterDiv.appendChild(newInput);
    return newInput.id;
  }

  static addText() {
    var masterDiv = document.getElementById('DynamicElements');
    var newTextArea = document.createElement("textarea");
    var i = this.inputCounter++;

    newTextArea.id = 'textarea' + i;
    newTextArea.className = 'test';
    newTextArea.placeholder = "Dynamic area #" + i + "!";

    masterDiv.appendChild(newTextArea);
    return newTextArea.id;
  }

  static addIFrame(loadCallback) {
    var masterDiv = document.getElementById('DynamicElements');
    var frame = document.createElement("iframe");
    var i = this.inputCounter++;

    frame.height = "100";
    frame.id = 'iframe' + i;
    if(loadCallback) {
      frame.addEventListener('load', function() {
        // Give KMW's attachment events a chance to run first.
        window.setTimeout(loadCallback, Math.max(100, 5000));
      });
    }
    frame.setAttribute("src", "resources/html/iframe.html");

    masterDiv.appendChild(frame);
    return frame.id;
  }

  static addDesignIFrame(loadCallback) {
    var masterDiv = document.getElementById('DynamicElements');
    var frame = document.createElement("iframe");
    var i = this.inputCounter++;

    frame.height = "100";
    frame.id = 'designIFrame' + i;
    frame.src = "resources/html/designIframe.html";

    if(loadCallback) {
      frame.addEventListener('load', function() {
        loadCallback();
      });
    }

    masterDiv.appendChild(frame);
    return frame.id;
  }

  static addEditable() {
    var masterDiv = document.getElementById('DynamicElements');
    var editable = document.createElement("div");
    var i = this.inputCounter++;

    editable.contentEditable = 'true';
    editable.textContent = "Edit me!";
    editable.id = 'editable' + i;
    editable.style.width="500px";

    masterDiv.appendChild(editable);
    return editable.id;
  }
}