import { CompilerOptions } from "@keymanapp/common-types";
import { LDMLKeyboardXMLSourceFileReaderOptions } from "@keymanapp/developer-utils";

/**
 * @public
 * Options for the .xml LDML keyboard compiler
 */
export interface LdmlCompilerOptions extends CompilerOptions {
  /**
   * Paths and other options required for reading .xml files
   */
  readerOptions: LDMLKeyboardXMLSourceFileReaderOptions;
};
