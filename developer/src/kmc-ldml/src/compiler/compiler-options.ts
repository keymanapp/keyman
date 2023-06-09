import { LDMLKeyboardXMLSourceFileReaderOptions } from "@keymanapp/common-types";

// TODO: inherit from common-types CompilerOptions, noting alternate names for debug and addCompilerVersion!
export interface CompilerOptions {
  /**
   * Add debug information to the .kmx file when compiling
   */
  debug?: boolean;

  /**
   * Add metadata about the compiler version to .kmx file when compiling
   */
  addCompilerVersion?: boolean;

  /**
   * Paths and other options required for reading .xml files
   */
  readerOptions: LDMLKeyboardXMLSourceFileReaderOptions;
};
