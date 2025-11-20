import { type AbstractElementTextStore }  from './abstractElementTextStore.js';
import { InputElementTextStore } from './inputTextStore.js';
import { TextAreaElementTextStore } from './textareaTextStore.js';
import { DesignIFrameElementTextStore } from './designIFrameTextStore.js';
import { ContentEditableElementTextStore } from './contentEditableTextStore.js';
import { nestedInstanceOf } from './nestedInstanceOf.js';

export function createTextStoreForElement(e: HTMLElement): AbstractElementTextStore<any> {
  // Complex type scoping is implemented here so that kmwutils.ts is not a dependency for test compilations.

  if(nestedInstanceOf(e, "HTMLInputElement")) {
    return new InputElementTextStore(<HTMLInputElement> e);
  } else if(nestedInstanceOf(e, "HTMLTextAreaElement")) {
    return new TextAreaElementTextStore(<HTMLTextAreaElement> e);
  } else if(nestedInstanceOf(e, "HTMLIFrameElement")) {
    const iframe = <HTMLIFrameElement> e;

    if(iframe.contentWindow && iframe.contentWindow.document && iframe.contentWindow.document.designMode == "on") {
      return new DesignIFrameElementTextStore(iframe);
    } else if (e.isContentEditable) {
      // Do content-editable <iframe>s make sense?
      return new ContentEditableElementTextStore(e);
    } else {
      return null;
    }
  } else if(e.isContentEditable) {
    return new ContentEditableElementTextStore(e);
  }

  return null;
}