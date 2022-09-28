import { LDMLKeyboard, KMXPlus } from "@keymanapp/common-types";
import CompilerCallbacks from "./callbacks.js";
import { SectionIdent } from '@keymanapp/ldml-keyboard-constants';

/* istanbul ignore next */
export class SectionCompiler {
  protected readonly keyboard: LDMLKeyboard.LKKeyboard;
  protected readonly callbacks: CompilerCallbacks;

  constructor(source: LDMLKeyboard.LDMLKeyboardXMLSourceFile, callbacks: CompilerCallbacks) {
    this.keyboard = source.keyboard;
    this.callbacks = callbacks;
  }

  public get id(): SectionIdent {
    return null;
  }

  public get required(): boolean {
    return true;
  }

  public compile(sections: KMXPlus.GlobalSections): KMXPlus.Section {
    return null;
  }

  public validate(): boolean {
    return true;
  }
}