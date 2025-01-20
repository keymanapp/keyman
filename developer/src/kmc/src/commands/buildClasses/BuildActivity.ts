import * as fs from 'fs';
import * as path from 'path';
import { KeymanFileTypes } from "@keymanapp/common-types";
import { CompilerCallbacks, CompilerOptions, KeymanCompiler } from "@keymanapp/developer-utils";
import { InfrastructureMessages } from '../../messages/infrastructureMessages.js';

export abstract class BuildActivity {
  public abstract get name(): string;
  public abstract get sourceExtension(): KeymanFileTypes.Source;
  public abstract get compiledExtension(): KeymanFileTypes.Binary;
  public abstract get description(): string;
  public abstract build(infile: string, outfile: string, callbacks: CompilerCallbacks, options: CompilerOptions): Promise<boolean>;

  protected async runCompiler<T extends CompilerOptions>(compiler: KeymanCompiler, infile: string, outfile: string, callbacks: CompilerCallbacks, options: T): Promise<boolean> {
    if(!await compiler.init(callbacks, options)) {
      return false;
    }

    const result = await compiler.run(infile, outfile);
    if(!result) {
      return false;
    }

    if(!this.createOutputFolder(outfile ?? infile, callbacks)) {
      return false;
    }

    return await compiler.write(result.artifacts);
  }

  private createOutputFolder(targetFilename: string, callbacks: CompilerCallbacks): boolean {
    const targetFolder = path.dirname(targetFilename);

    try {
      fs.mkdirSync(targetFolder, {recursive: true});
    } catch(e) {
      callbacks.reportMessage(InfrastructureMessages.Error_CannotCreateFolder({folderName:targetFolder, e}));
      return false;
    }

    return true;
  }
};