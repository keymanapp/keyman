// Defines an object for dynamically adding elements for testing purposes.
// Designed for use with the robustAttachment.html fixture.

export class DynamicElements {
  static inputCounter = 0;

  static addInput() {
    const masterDiv = document.getElementById('DynamicElements');
    const newInput = document.createElement("input");
    const i = this.inputCounter++;

    newInput.id = 'input' + i;
    newInput.className = 'test';
    newInput.placeholder = "Dynamic area #" + i + "!";

    masterDiv.appendChild(newInput);
    return newInput.id;
  }

  static addText() {
    const masterDiv = document.getElementById('DynamicElements');
    const newTextArea = document.createElement("textarea");
    const i = this.inputCounter++;

    newTextArea.id = 'textarea' + i;
    newTextArea.className = 'test';
    newTextArea.placeholder = "Dynamic area #" + i + "!";

    masterDiv.appendChild(newTextArea);
    return newTextArea.id;
  }

  static addIFrame(loadCallback: any) {
    const masterDiv = document.getElementById('DynamicElements');
    const frame = document.createElement("iframe");
    const i = this.inputCounter++;

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

  static addDesignIFrame(loadCallback: any) {
    const masterDiv = document.getElementById('DynamicElements');
    const frame = document.createElement("iframe");
    const i = this.inputCounter++;

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
    const masterDiv = document.getElementById('DynamicElements');
    const editable = document.createElement("div");
    const i = this.inputCounter++;

    editable.contentEditable = 'true';
    editable.textContent = "Edit me!";
    editable.id = 'editable' + i;
    editable.style.width="500px";

    masterDiv.appendChild(editable);
    return editable.id;
  }
}