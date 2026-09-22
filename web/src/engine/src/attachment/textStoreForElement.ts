/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import { AbstractElementTextStore } from 'keyman/engine/element-text-stores';
import { verifyElementIsInputElement } from './verifyElementIsInputElement.js';

/**
 * Given a element, this function determines the corresponding TextStore
 * @param elem  The element to get the TextStore for.
 * @returns     The TextStore for the element, or null if the element is not
 *              an input element or doesn't have KMW attached to it.
 */
export function textStoreForElement(elem: HTMLElement): AbstractElementTextStore<any> {
  elem = verifyElementIsInputElement(elem);
  if (elem == null) {
    return null;
  }

  // Step 2:  With the most likely host element determined, obtain the corresponding TextStore
  // instance.
  return elem._kmwAttachment?.textStore;
}
