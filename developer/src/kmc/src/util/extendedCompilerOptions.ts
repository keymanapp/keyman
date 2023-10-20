import { CompilerOptions } from '@keymanapp/common-types';

export interface ExtendedCompilerOptions extends CompilerOptions {
  /**
   * Verify that the project meets the requirements of the keymanapp/keyboards
   * or keymanapp/lexical-models repository, e.g. verify that project license is
   * MIT
   */
  forPublishing?: boolean;
};
