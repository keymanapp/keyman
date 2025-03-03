import { CompilerEvent, EventResolver } from "@keymanapp/developer-utils";
import { LdmlCompilerMessages } from "./ldml-compiler-messages.js";

export class LdmlEventResolver implements EventResolver {

  contentsCache: Map<string, string> = new Map();

  /** add or update a file */
  addFile(filename: string, xml: any) {
    if (typeof xml !== 'string') {
      xml = xml.toString();
    }
    this.contentsCache.set(filename, xml);
  }

  resolve(event: CompilerEvent) {
    if (event.offset && !event.line && event.filename) {
      LdmlCompilerMessages.resolveLineNumber(event, this.contentsCache.get(event.filename));
    }
  }
}
