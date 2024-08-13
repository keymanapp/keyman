import { LDMLKeyboard, KMXPlus, CompilerCallbacks } from "@keymanapp/common-types";
import { SectionIdent, constants } from '@keymanapp/ldml-keyboard-constants';

/** newable interface to SectionCompiler c'tor */
export type SectionCompilerNew = new (source: LDMLKeyboard.LDMLKeyboardXMLSourceFile, callbacks: CompilerCallbacks) => SectionCompiler;
export abstract class SectionCompiler {
  protected readonly keyboard3: LDMLKeyboard.LKKeyboard;
  protected readonly callbacks: CompilerCallbacks;

  constructor(source: LDMLKeyboard.LDMLKeyboardXMLSourceFile, callbacks: CompilerCallbacks) {
    this.keyboard3 = source.keyboard3;
    this.callbacks = callbacks;
  }

  public abstract get id(): SectionIdent;

  /**
   * This is called before compile.
   * @returns false if this compiler failed to validate.
   */
  public validate(): boolean {
    return true;
  }

  /**
   * Perform the compilation for this section, returning the correct Section subclass
   * object.
   *
   * @param sections any declared dependency sections per dependencies()
   */
  public abstract compile(sections: KMXPlus.DependencySections): KMXPlus.Section;

  /**
   * This is called after all other compile phases have completed,
   * when being called by validate(), and provides an
   * opportunity for late error reporting, for example for invalid strings.
   * @param section the compiled section, if any.
   * @returns false if validate fails
   */
  public postValidate(section?: KMXPlus.Section): boolean {
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
