/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by srl on 2025-03-03
 *
 * Implementation of a hook on LDML events
 */

import { LineFinderCache, FileConsumer } from "../../line-utils.js";
import { CompilerEvent, EventResolver } from "../../index.js";
export class LineFinderEventResolver implements EventResolver, FileConsumer {
  private lfcache = new LineFinderCache();

  addFile(filename: string, contents: string): void {
    this.lfcache.addFile(filename, contents);
  }

  /** resolve a CompilerEvent by expanding the line numbers */
  resolve(event: CompilerEvent) {
    if (event.offset && !event.line && event.filename) {
      const lf = this.lfcache.getByFilename(event.filename);
      const loc = lf.findOffset(event.offset);
      event.line = loc.line;
      event.column = loc.column;
    }
    return event;
  }
}
