import { CompilerCallbacks, CompilerOptions, KeymanFileTypes } from "@keymanapp/common-types";
import { escapeRegExp } from "../../util/escapeRegExp.js";

export abstract class BuildActivity {
  public abstract get name(): string;
  public abstract get sourceExtension(): KeymanFileTypes.Source;
  public abstract get compiledExtension(): KeymanFileTypes.Binary;
  public abstract get description(): string;
  public abstract build(infile: string, outfile: string, callbacks: CompilerCallbacks, options: CompilerOptions): Promise<boolean>;
  protected getOutputFilename(infile: string, outfile?: string): string {
    return outfile ??
      infile.replace(new RegExp(escapeRegExp(this.sourceExtension), "g"), this.compiledExtension);
  }
};