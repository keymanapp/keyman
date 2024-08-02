import { CompilerCallbacks, KMXPlus } from "@keymanapp/common-types";
import { LDMLKeyboard } from "@keymanapp/developer-utils";

/** newable interface to Linter c'tor */
export type LinterNew = new (source: LDMLKeyboard.LDMLKeyboardXMLSourceFile, kmx: KMXPlus.KMXPlusFile, callbacks: CompilerCallbacks) => Linter;
/** Abstract interface for a class which provides additional hints against a compiled keyboard file */
export abstract class Linter {
  protected readonly keyboard3: LDMLKeyboard.LKKeyboard;
  protected readonly callbacks: CompilerCallbacks;
  protected readonly kmx: KMXPlus.KMXPlusFile;

  constructor(source: LDMLKeyboard.LDMLKeyboardXMLSourceFile, kmx: KMXPlus.KMXPlusFile, callbacks: CompilerCallbacks) {
    this.keyboard3 = source.keyboard3;
    this.callbacks = callbacks;
    this.kmx = kmx;
  }

  /**
   * Check the file for any additional needed hints.
   * Add the hints to the callbacks.
   * @returns false on catastrophic failure of the linter.
   */
  public abstract lint() : Promise<boolean>;
}
