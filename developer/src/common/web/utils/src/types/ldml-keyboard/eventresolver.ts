/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by srl on 2025-03-03
 *
 * Implementation of a hook on LDML events
 */

import { CompilerEvent, EventResolver, LineFinder } from "../../index.js";
export class LdmlEventResolver implements EventResolver {

  contentsCache: Map<string, LineFinder> = new Map();

  /** add or update a source file */
  addFile(filename: string, xml: any) {
    this.contentsCache.set(filename, new LineFinder(xml));
  }

  /** resolve a CompilerEvent by expanding the line numbers */
  resolve(event: CompilerEvent) {
    if (event.offset && !event.line && event.filename) {
      const lf = this.contentsCache.get(event.filename);
      const loc = lf.findOffset(event.offset);
      event.line = loc.line;
      event.column = loc.column;
    }
    return event;
  }
}
