interface Document {
    selection: any
}

interface Element {
    dir: string,                        // ltr vs rtl directionality.
    _kmwAttachment: Element,            // Used to track each input element's attachment data.
    offsetLeft: number,     // Not detected by TS?
    offsetTop: number,      // Not detected by TS?
    shim: Element,                      // Used in subkey elements for smooth fading.
    styleSheet: CSSStyleDeclaration,    // Extension of IE.
    type: string,           // Not detected by TS?
    value: string           // Not detected by TS?

    // Touch element extensions
    base: Element,              // Used by touch alias elements.
    rows: number,
    style: any,
    _kmwResizeHandler: (e: any) => void,

    // Used by our util.wait / util.alert system
    dismiss: () => void
}

interface CSSStyleDeclaration {
    MozBoxSizing: any
}