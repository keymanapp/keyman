/**
 * Checks the type of an input DOM-related object while ensuring that it
 * is checked against the correct prototype, as class prototypes are (by
 * specification) scoped upon the owning Window.
 *
 * See
 * https://stackoverflow.com/questions/43587286/why-does-instanceof-return-false-on-chrome-safari-and-edge-and-true-on-firefox
 * for more details.
 *
 * @param {EventTarget}   elem       An element of the web page or one of
 *                                   its IFrame-based subdocuments.
 * @param {string}        className  The plain-text name of the expected
 *                                   Element type.
 * @return {boolean}
 */
export function nestedInstanceOf(elem: EventTarget, className: string): boolean {
  let scopedClass;

  if(!elem) {
    // If we're bothering to check something's type, null references don't match
    // what we're looking for.
    return false;
  }
  // @ts-ignore
  if (elem['Window']) { // Window objects contain the class definitions for types held within them.  So, we can check for those.
    return className == 'Window';
    // @ts-ignore
  } else if (elem['defaultView']) { // Covers Document.
    // @ts-ignore
    scopedClass = (elem as Document)['defaultView'][className];
    // @ts-ignore
  } else if(elem['ownerDocument']) {
    // @ts-ignore
    scopedClass = (elem as Node).ownerDocument.defaultView[className];
  }

  if(scopedClass) {
    return elem instanceof scopedClass;
  } else {
    return false;
  }
}
