/*
 * Keyman is copyright (C) SIL Global. MIT License.
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
   * Target file format extension, required, e.g. '.kmn'
   */
  target: string;

  /**
   * Fail if the keyboard conversion is not 100% complete
   */
  failIfIncomplete?: boolean;
};
