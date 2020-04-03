/// <reference path="outputTarget.ts" />
// Defines a basic HTMLInputElement wrapper.
///<reference path="input.ts" />
// Defines a basic HTMLTextAreaElement wrapper.
///<reference path="textarea.ts" />
// Defines a basic content-editable wrapper.
///<reference path="contentEditable.ts" />
// Defines a basic design-mode IFrame wrapper.
///<reference path="designIFrame.ts" />
// Defines a basic touch-alias element wrapper.
///<reference path="touchAlias.ts" />

namespace com.keyman.dom.targets {
  export function wrapElement(e: HTMLElement): OutputTarget {
    // Complex type scoping is implemented here so that kmwutils.ts is not a dependency for test compilations.

    if(Utils.instanceof(e, "HTMLInputElement")) {
      return new Input(<HTMLInputElement> e);
    } else if(Utils.instanceof(e, "HTMLTextAreaElement")) {
      return new TextArea(<HTMLTextAreaElement> e);
    } else if(Utils.instanceof(e, "TouchAliasElement")) {
      return new TouchAlias(<TouchAliasElement> e);
    } else if(Utils.instanceof(e, "HTMLIFrameElement")) {
      let iframe = <HTMLIFrameElement> e;

      if(iframe.contentWindow && iframe.contentWindow.document && iframe.contentWindow.document.designMode == "on") {
        return new DesignIFrame(iframe);
      } else if (e.isContentEditable) {
        // Do content-editable <iframe>s make sense?
        return new ContentEditable(e);
      } else {
        return null;
      }
    } else if(e.isContentEditable) {
      return new ContentEditable(e);
    }
    
    return null;
  }
}