import { CompilerOptions, LDMLKeyboardXMLSourceFileReaderOptions } from "@keymanapp/developer-utils";
import { KMXPlusVersion } from "@keymanapp/ldml-keyboard-constants";

export type LdmlKeyboardVersion = KMXPlusVersion;

/**
 * @public
 * Options for the .xml LDML keyboard compiler
 */
export interface LdmlCompilerOptions extends CompilerOptions {
  /**
   * Paths and other options required for reading .xml files
   */
  readerOptions: LDMLKeyboardXMLSourceFileReaderOptions;
  version?: LdmlKeyboardVersion;    // used in v19+
};
