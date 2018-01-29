// Defines a number of KMW objects.
/// <reference path="kmwtypedefs.ts"/>

interface Window {
    attachEvent: (eventNameWithOn:string, callback: () => void) => void,
    clientWidth: number
}

interface Document {
    parentWindow: Window,                  // Thanks again, IE.
    selection: any,                     // Is IE only; TS ignores it because of this.

    _KeymanWebSelectionStart: number,
    _KeymanWebSelectionEnd: number
}

interface Element {
    _kmwAttachment: AttachmentInfo,                // Used to track each input element's attachment data.
    shim: HTMLElement,                  // Used in subkey elements for smooth fading.

    _KeymanWebSelectionStart: number,
    _KeymanWebSelectionEnd: number,

    styleSheet: CSSStyleDeclaration,    // Extension of IE.  TS ignores it because of this.
    attachEvent: any,
    detachEvent: any,
    unSelectable: string,
    currentStyle: any,

    // Touch element extensions
    base: HTMLElement,                  // Refers to the aliased element.  Is a property of the alias.
    disabled: boolean,
    kmwInput: boolean,
    _kmwResizeHandler: (e: any) => void,

    // Used by our util.wait / util.alert system
    dismiss: () => void
}

interface CSSStyleDeclaration {
    MozBoxSizing: any,
    MozUserSelect: any,
    KhtmlUserSelect: any,
    UserSelect: any,
    WebkitUserSelect: any
}

interface HTMLStyleElement {
    filter: any,                        // More IE-specific fields.
    zoom: any
}

interface KeyboardEvent {
    _kmw_block: boolean                 // Needed to prevent an old Firefox bug.
}