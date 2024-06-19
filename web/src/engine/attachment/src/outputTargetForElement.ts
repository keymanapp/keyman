import { nestedInstanceOf } from "keyman/engine/element-wrappers";

/**
 * Given a DOM event related to an KMW-attached element, this function determines
 * the corresponding OutputTarget.
 * @param e
 * @returns
 */
export function eventOutputTarget(e: Event) {
  let Ltarg: HTMLElement = e?.target as HTMLElement;
  return outputTargetForElement(Ltarg);
}

/**
 * Given a DOM event related to an KMW-attached element, this function determines
 * the corresponding OutputTarget.
 * @param e
 * @returns
 */
export function outputTargetForElement(Ltarg: HTMLElement) {
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

  // Step 2:  With the most likely host element determined, obtain the corresponding OutputTarget
  // instance.
  return Ltarg._kmwAttachment?.interface;
}