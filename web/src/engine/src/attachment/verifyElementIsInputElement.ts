/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import { nestedInstanceOf } from "keyman/engine/element-text-stores";

/**
 * Verify that the given element is a valid input element for KMW
 * attachment, and return the element if so. Otherwise, return null.
 * @param  elem
 * @returns The verified element, or null if the element is not a valid
 * input element.
 */
export function verifyElementIsInputElement(elem: HTMLElement): HTMLElement | null {
  if (elem == null) {
    return null;
  }
  // ... determine the element expected to hold the KMW attachment object based on
  // its typing, properties, etc.

  // @ts-ignore
  if (elem['body']) {
    // @ts-ignore
    elem = elem['body']; // Occurs in Firefox for design-mode iframes.
  }

  if (elem.nodeType == 3) { // defeat Safari bug
    elem = elem.parentNode as HTMLElement;
  }

  // Verify that the element does correspond to a remappable input field
  if (nestedInstanceOf(elem, "HTMLInputElement")) {
    const et = (elem as HTMLInputElement).type.toLowerCase();
    if (!(et == 'text' || et == 'search')) {
      return null;
    }
  }

  return elem;
}

