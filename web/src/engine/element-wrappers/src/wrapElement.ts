import type OutputTarget from './outputTarget.js';
import Input from './input.js';
import TextArea from './textarea.js';
import DesignIFrame from './designIFrame.js';
import ContentEditable from './contentEditable.js';
import { nestedInstanceOf } from './utils.js';

export default function wrapElement(e: HTMLElement): OutputTarget<any> {
  // Complex type scoping is implemented here so that kmwutils.ts is not a dependency for test compilations.

  if(nestedInstanceOf(e, "HTMLInputElement")) {
    return new Input(<HTMLInputElement> e);
  } else if(nestedInstanceOf(e, "HTMLTextAreaElement")) {
    return new TextArea(<HTMLTextAreaElement> e);
  } else if(nestedInstanceOf(e, "HTMLIFrameElement")) {
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