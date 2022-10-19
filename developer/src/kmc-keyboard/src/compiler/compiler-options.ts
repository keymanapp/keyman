

export default interface CompilerOptions {
  /**
   * Add debug information to the .kmx file when compiling
   */
  debug: boolean;

  /**
   * Add metadata about the compiler version to .kmx file when compiling
   */
  addCompilerVersion: boolean;
};
