import KMXPlusFile from "../kmx/kmx-plus";
import LDMLKeyboardXMLSourceFile from "../ldml-keyboard/ldml-keyboard-xml";
import CompilerCallbacks from "./callbacks";

export class SectionCompiler {
  private _kmx: KMXPlusFile;
  private _source: LDMLKeyboardXMLSourceFile;
  private _callbacks: CompilerCallbacks;

  constructor(kmx: KMXPlusFile, source: LDMLKeyboardXMLSourceFile, callbacks: CompilerCallbacks) {
    this._kmx = kmx;
    this._source = source;
  }

  get kmx() { return this._kmx; }
  get source() { return this._source; }
  get callbacks() { return this._callbacks; }
}