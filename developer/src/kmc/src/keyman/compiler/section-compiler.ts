import { GlobalSections, Section } from "../kmx/kmx-plus";
import LDMLKeyboardXMLSourceFile, { LKKeyboard } from "../ldml-keyboard/ldml-keyboard-xml";
import CompilerCallbacks from "./callbacks";
import { SectionIdent } from '@keymanapp/ldml-keyboard-constants';

export class SectionCompiler {
  protected readonly keyboard: LKKeyboard;
  protected readonly callbacks: CompilerCallbacks;

  constructor(source: LDMLKeyboardXMLSourceFile, callbacks: CompilerCallbacks) {
    this.keyboard = source.keyboard;
    this.callbacks = callbacks;
  }

  /* istanbul ignore next */
  public get id(): SectionIdent {
    return null;
  }

  /* istanbul ignore next */
  public get required(): boolean {
    return true;
  }

  /* istanbul ignore next */
  public compile(sections: GlobalSections): Section {
    return null;
  }

  public validate(): boolean {
    return true;
  }
}