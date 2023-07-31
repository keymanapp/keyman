import { CompilerCallbacks } from "@keymanapp/common-types";
import { escapeRegExp } from "../../util/escapeRegExp.js";

export interface BuildActivityOptions {
  debug?: boolean;
  outFile?: string;
  compilerVersion?: boolean;
  warnDeprecatedCode?: boolean;
  compilerWarningsAsErrors?: boolean;
};

export abstract class BuildActivity {
  public abstract get name(): string;
  public abstract get sourceExtension(): string;
  public abstract get compiledExtension(): string;
  public abstract get description(): string;
  public abstract build(infile: string, callbacks: CompilerCallbacks, options: BuildActivityOptions): Promise<boolean>;
  protected getOutputFilename(infile: string, options: BuildActivityOptions): string {
    return options.outFile ?
      options.outFile :
      infile.replace(new RegExp(escapeRegExp(this.sourceExtension), "g"), this.compiledExtension);
  }
};