/**
 * Checks the type of an input DOM-related object while ensuring that it is checked against the correct prototype,
 * as class prototypes are (by specification) scoped upon the owning Window.
 *
 * See https://stackoverflow.com/questions/43587286/why-does-instanceof-return-false-on-chrome-safari-and-edge-and-true-on-firefox
 * for more details.
 *
 * @param {Element|Event}   Pelem       An element of the web page or one of its IFrame-based subdocuments.
 * @param {string}          className   The plain-text name of the expected Element type.
 * @return {boolean}
 */
export function nestedInstanceOf(Pelem: Event|EventTarget, className: string): boolean {
  var scopedClass;

  if(!Pelem) {
    // If we're bothering to check something's type, null references don't match
    // what we're looking for.
    return false;
  }
  if (Pelem['Window']) { // Window objects contain the class definitions for types held within them.  So, we can check for those.
    return className == 'Window';
  } else if (Pelem['defaultView']) { // Covers Document.
    scopedClass = Pelem['defaultView'][className];
  } else if(Pelem['ownerDocument']) {
    scopedClass = (Pelem as Node).ownerDocument.defaultView[className];
  } else if(Pelem['target']) {
    var event = Pelem as Event;

    if(this.instanceof(event.target, 'Window')) {
      scopedClass = event.target[className];
    } else if(this.instanceof(event.target, 'Document')) {
      scopedClass = (event.target as Document).defaultView[className];
    } else if(this.instanceof(event.target, 'HTMLElement')) {
      scopedClass = (event.target as HTMLElement).ownerDocument.defaultView[className];
    }
  }

  if(scopedClass) {
    return Pelem instanceof scopedClass;
  } else {
    return false;
  }
}