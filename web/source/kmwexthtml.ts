// Defines a number of KMW objects.
/// <reference path="kmwtypedefs.ts"/>

interface Document {
    _KeymanWebSelectionStart: number,
    _KeymanWebSelectionEnd: number
}

interface Element {
    _kmwAttachment: AttachmentInfo,     // Used to track each input element's attachment data.
    shim: HTMLElement,                  // Used in subkey elements for smooth fading.

    _KeymanWebSelectionStart: number,
    _KeymanWebSelectionEnd: number,

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
    // For legacy 'selection' management.
    MozUserSelect: any,
    KhtmlUserSelect: any,
    UserSelect: any,
    WebkitUserSelect: any
}

interface KeyboardEvent {
    _kmw_block: boolean                 // Needed to prevent an old Firefox bug.
}