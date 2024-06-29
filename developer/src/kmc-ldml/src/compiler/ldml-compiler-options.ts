import { CompilerOptions, LDMLKeyboardXMLSourceFileReaderOptions } from "@keymanapp/common-types";

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
