export interface BuildActivityOptions {
  debug?: boolean;
  outFile?: string;
  compilerVersion?: boolean;
};

export abstract class BuildActivity {
  public abstract get name(): string;
  public abstract get sourceExtension(): string;
  public abstract get compiledExtension(): string;
  public abstract get description(): string;
  public abstract build(infile: string, options: BuildActivityOptions): Promise<boolean>;
};