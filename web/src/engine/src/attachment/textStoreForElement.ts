import { nestedInstanceOf } from "keyman/engine/element-text-stores";

/**
 * Given a DOM event related to an KMW-attached element, this function determines
 * the corresponding TextStore.
 * @param e
 * @returns
 */
export function textStoreForElement(Ltarg: HTMLElement) {
  if (Ltarg == null) {
    return null;
  }
  // ... determine the element expected to hold the KMW attachment object based on
  // its typing, properties, etc.

  // @ts-ignore
  if(Ltarg['body']) {
    // @ts-ignore
    Ltarg = Ltarg['body']; // Occurs in Firefox for design-mode iframes.
  }

  if (Ltarg.nodeType == 3) { // defeat Safari bug
    Ltarg = Ltarg.parentNode as HTMLElement;
  }

  // Verify that the element does correspond to a remappable input field
  if(nestedInstanceOf(Ltarg, "HTMLInputElement")) {
    const et=(Ltarg as HTMLInputElement).type.toLowerCase();
    if(!(et == 'text' || et == 'search')) {
      return null;
    }
  }

  // Step 2:  With the most likely host element determined, obtain the corresponding TextStore
  // instance.
  return Ltarg._kmwAttachment?.textStore;
}