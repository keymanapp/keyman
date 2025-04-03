import { CompilerEvent, EventResolver, KeymanXMLReader } from "../../index.js";

export class LdmlEventResolver implements EventResolver {

  contentsCache: Map<string, number[]> = new Map();

  /** add or update a file */
  addFile(filename: string, xml: any) {
    this.contentsCache.set(filename, KeymanXMLReader.textToLines(xml));
  }

  resolve(event: CompilerEvent) {
    if (event.offset && !event.line && event.filename) {
      const lines = this.contentsCache.get(event.filename);
      const loc = KeymanXMLReader.offsetToLineColumn(event.offset, lines);
      event.line = loc.line;
      event.column = loc.column;
    }
    return event;
  }
}
