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

  /* c8 ignore next 11 */
  public get id(): SectionIdent {
    throw Error(`Internal Error: id() not implemented`);
  }

  public compile(sections: KMXPlus.DependencySections): KMXPlus.Section {
    throw Error(`Internal Error: compile() not implemented`);
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
      constants.section.elem,
      constants.section.vars,
    ]);
    return defaults;
  }
}
