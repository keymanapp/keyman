/**
 * Markdown transform for our `description` field. Tweaked to disable all inline
 * HTML, because we want descriptions to be short and sweet, and don't need any
 * of the more complex formatting that inline HTML affords.
 */

//
// Note: using marked 7.0.0.
// https://github.com/markedjs/marked/issues/2926
//
// Version 7.0.1 introduced a TypeScript 5.0+ feature `export type *` which causes:
//
//   ../../../node_modules/marked/lib/marked.d.ts:722:5 - error TS1383: Only named exports may use 'export type'.
//   722     export type * from "MarkedOptions";
//           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//
// https://github.com/markedjs/marked/compare/v7.0.0...v7.0.1#diff-32d87a2bc59f429470ccf7afc8ae8818914d4d45c3f7c5b1767c6a0a240b55c9R449-R451
//
// When we move to TS 5.0, we can upgrade marked.
//
import { Marked } from 'marked';

/*
  Markdown rendering: we don't want to use the global object, because this
  pollutes the settings for all modules. So we construct our own instance,
  so that we can strip all inline HTML; we don't need any
*/

const renderer = {
  html(_html:string, _block:boolean) {
    // we don't allow inline HTML
    return '';
  }
}
const markedStripHtml = new Marked({renderer});
const marked = new Marked();

/**
 *
 * @param markdown
 * @param allowHTML
 * @returns
 */
export function markdownToHTML(markdown: string, allowHTML: boolean): string {
  // <string>: .parse can return a Promise if async=true. We don't pass ths
  // option, and this sync usage isn't separated out in the types for
  // Marked.prototype.parse, so <string> avoids tsc complaints here.
  const html = <string> (allowHTML ? marked : markedStripHtml).parse(markdown.trim());
  return html;
}
