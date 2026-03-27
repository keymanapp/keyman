/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Converter options
 */
import { CompilerOptions } from "@keymanapp/developer-utils";

/**
 * @public
 * Options for the keyboard converter
 */
export interface ConverterOptions extends CompilerOptions {
  /**
   * Fail if the keyboard conversion is not 100% complete
   */
  failIfIncomplete?: boolean;
};
