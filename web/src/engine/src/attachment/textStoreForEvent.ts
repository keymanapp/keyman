import { textStoreForElement } from './textStoreForElement.js';

/**
 * Given a DOM event related to an KMW-attached element, this function determines
 * the corresponding TextStore.
 * @param e
 * @returns
 */

export function textStoreForEvent(e: Event) {
  const Ltarg: HTMLElement = e?.target as HTMLElement;
  return textStoreForElement(Ltarg);
}
