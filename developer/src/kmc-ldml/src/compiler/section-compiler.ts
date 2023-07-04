import { LDMLKeyboard, KMXPlus, CompilerCallbacks } from "@keymanapp/common-types";
import { SectionIdent, constants } from '@keymanapp/ldml-keyboard-constants';

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

  public compile(sections: KMXPlus.DependencySections): KMXPlus.Section {
    return null;
  }

  public validate(): boolean {
    return true;
  }

  /**
   * Get the dependencies for this compiler.
   * @returns set of dependent sections
   */
  public get dependencies(): Set<SectionIdent> {
    const defaults = new Set(<SectionIdent[]>[
      constants.section.strs,
      constants.section.list,
      constants.section.elem
    ]);
    return defaults;
  }
}
