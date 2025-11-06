/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import { type AbstractElementTextStore }  from './abstractElementTextStore.js';
import { InputElementTextStore } from './inputElementTextStore.js';
import { TextAreaElementTextStore } from './textAreaElementTextStore.js';
import { DesignIFrameElementTextStore } from './designIFrameElementTextStore.js';
import { ContentEditableElementTextStore } from './contentEditableElementTextStore.js';
import { nestedInstanceOf } from './utils.js';

/**
 * Wraps an HTMLElement in a concrete text-store implementation.
 *
 * @param e - The HTMLElement to create a text-store for.
 * @returns A concrete AbstractElementTextStore for the element, or null if the element
 *          type is not supported or no suitable store can be created.
 */
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