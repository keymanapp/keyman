import { Section } from "../kmx/kmx-plus";
import LDMLKeyboardXMLSourceFile from "../ldml-keyboard/ldml-keyboard-xml";
import CompilerCallbacks from "./callbacks";
import { SectionIdent } from '@keymanapp/ldml-keyboard-constants';

export class SectionCompiler {
  protected readonly source: LDMLKeyboardXMLSourceFile;
  protected readonly callbacks: CompilerCallbacks;

  constructor(source: LDMLKeyboardXMLSourceFile, callbacks: CompilerCallbacks) {
    this.source = source;
    this.callbacks = callbacks;
  }

  public get id(): SectionIdent {
    return null;
  }

  public compile(): Section {
    return null;
  }

  public validate(): boolean {
    return true;
  }
}