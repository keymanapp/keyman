// Defines a number of KMW objects.
/// <reference path="kmwtypedefs.ts"/>

interface Window {
    // DOM type prototypes
    HTMLElement:         typeof HTMLElement;
    HTMLTextAreaElement: typeof HTMLTextAreaElement;
    HTMLInputElement:    typeof HTMLInputElement;
    HTMLIFrameElement:   typeof HTMLIFrameElement;
    Document:            typeof Document;
    Event:               typeof Event;
    MouseEvent:          typeof MouseEvent;
    TouchEvent:          typeof TouchEvent;
}

interface Element {
    _kmwAttachment: com.keyman.AttachmentInfo,     // Used to track each input element's attachment data.
    shim: HTMLElement,                  // Used in subkey elements for smooth fading.

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
    MozUserSelect: any, // Not necessary with Firefox 52+, which was released... in 2017.
    KhtmlUserSelect: any, // No dating information for the rest at present.
    UserSelect: any,
    WebkitUserSelect: any,
    WebkitOverflowScrolling?: string,
    msTransition?: string,
    MozTransition?: string,
    WebkitTransition?: string
}

interface TouchEvent {
  pageY?: number,
  clientX?: number,
  clientY?: number
}