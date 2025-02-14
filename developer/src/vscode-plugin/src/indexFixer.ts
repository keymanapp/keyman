/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

const jsdom = require("jsdom");
const { JSDOM } = jsdom;

export function fixupUrl(s: string, buildUri: string) : string {
    if (s.startsWith('/')) {
        // trim off leading /
        s = s.slice(1);
    } else {
        throw Error(`Fixup: URL ${s} didn't start with ./`);
    }
    if (!buildUri.endsWith('/')) {
        buildUri = buildUri + '/';
    }
    const u : URL = new URL(s, buildUri);
    return u.toString();
}

export function fixupElementAttribute(e : Element, attr: string, buildUri: string) : Element {
    const v = e.getAttribute(attr);
    if (!v) {
        throw Error(`Could not find attribute ${attr}= on ${e.outerHTML}`);
    }
    const n = fixupUrl(v, buildUri);
    e.setAttribute(attr, n);
    return e;
}

export function fixupElementByTag(doc: Document, tag: string, attr: string, buildUri: string) {
    const list = doc.getElementsByTagName(tag);
    if (!list.length) {
        throw Error(`Expected at least one <${tag}/> but found ${list.length}`);
    }
    for (const e of list) {
        fixupElementAttribute(e, attr, buildUri);
    }
}

/**
 *
 * @param html index.html source
 * @param buildUri index to extensionUri/build
 * @param nonce to replace `@NONCE@`
 * @param cspSource to replace `@CSPSOURCE@`
 */
export function fixupIndexHtml(html : string, buildUri : string, nonce : string, cspSource : string) : string {
    const dom = new JSDOM(html);
    const doc = dom.window.document;
    fixupElementByTag(doc, 'link', 'href', buildUri);
    fixupElementByTag(doc, 'script', 'src', buildUri);
    // serialize, and substitute
    return doc.documentElement.outerHTML
        .replace(/@NONCE@/g, nonce)
        .replace(/@CSPSOURCE@/g, cspSource);
}
