import * as fs from 'fs';
import path from 'path';
import convertSourceMap from 'convert-source-map'; // Transforms sourcemaps among various common formats.
                                                   // Base64, stringified-JSON, end-of-file comment...

// Because we're compiling the main project based on its TS build outputs, and our cross-module references
// also link to build outputs, we need to clean up the sourcemap-paths.  Also, the individual module sourcemaps.

export interface Mapping {
  readonly from: RegExp | string;
  readonly to:   string;
}

/**
 * An intermediate state for sourcemap source path remapping operations + functions to
 * mutate it.
 */
class RemappingState {
  constructor(srcmap: Object) {
    this.sourceMap = srcmap;
    this.resetChangedSourcepathTracking();
  }

  public readonly sourceMap: any;
  private _unchangedSourcePaths: string[] = [];

  public get unchangedSourcepaths() {
    return this._unchangedSourcePaths;
  }

  public resetChangedSourcepathTracking() {
    this._unchangedSourcePaths = [...this.sourceMap.sources];
  }

  /**
   * The sourcemap's `sourceRoot` property.
   */
  public get sourceRoot() {
    return this.sourceMap.sourceRoot;
  }

  /**
   * The sourcemap's `sourceRoot` property.
   */
  public set sourceRoot(path: string) {
    this.sourceMap.sourceRoot = path;
  }

  /**
   * Searches for source-file paths that match specified path-remapping rules and remaps them
   * accordingly.
   * @param transforms Path-remapping rules to apply.  For each remappable path, only the first
   * matching rule in the specified set will be applied.
   * @returns `this` - the original instance - in order to facilitate function-call chaining.
   */
  remapPaths(transforms: Mapping | Mapping[], logger?: (from: string, to: string) => void) {
    if(!(transforms instanceof Array)) {
      transforms = [transforms];
    }

    const unusedTransforms = [...transforms];

    const sourcePaths = this.sourceMap.sources;
    for(let i = 0; i < sourcePaths.length; i++) {
      for(const transform of transforms) {
        if(sourcePaths[i].match(transform.from)) {
          // Note that this source path has been remapped (if it wasn't previously)
          if(this.unchangedSourcepaths.includes(sourcePaths[i])) {
            this.unchangedSourcepaths.splice(this.unchangedSourcepaths.indexOf(sourcePaths[i]), 1);
          }

          // Note that this transform has been used.
          if(unusedTransforms.includes(transform)) {
            unusedTransforms.splice(unusedTransforms.indexOf(transform), 1);
          }

          let startPath = sourcePaths[i];
          sourcePaths[i] = sourcePaths[i].replace(transform.from, transform.to);
          if(logger) {
            logger(startPath, sourcePaths[i]);
          }
          break;
        }
      }
    }

    if(unusedTransforms.length > 0) {
      console.warn();
      console.warn("Unused sourcemath mappings:");
      for(const transform of unusedTransforms) {
        let isRegex = transform.from instanceof RegExp;
        let delim = isRegex ? '' : '"';
        console.warn(`* ${delim}${transform.from.toString()}${delim} => "${transform.to}"`);
      }
    }

    return this;
  }

  /**
   * Prepends all `sources` paths with the current sourceRoot (or just the part after a
   * specified index).
   * @param index
   */
  public normalizeWithSourceRoot(index?: number) {
    const sources = this.sourceMap.sources;
    const originalSrcRoot = this.sourceRoot;

    index = index || 0;
    const prefix = originalSrcRoot.substring(index);
    this.sourceRoot = originalSrcRoot.substring(0, index);

    for(let i=0; i < sources.length; i++) {
      // Normalizes, then swaps Windows '\\' entries with '/' if they exist;
      // path.normalize respects Windows too much for our taste here.
      sources[i] = path.normalize(`${prefix}/${sources[i]}`).replace(/\\/g, '/');
    }
  }

  /**
   * Writes the current state of the remapped sourcemap out to the specified file.
   * @param file
   */
  public toFile(file: fs.PathLike) {
    fs.writeFileSync(file, convertSourceMap.fromObject(this.sourceMap).toJSON(2)); // 2 = space count
  }
}

export default class SourcemapRemapper {
  private constructor() {}

  /**
   * Starts sourcemap source path remapping from a pre-loaded JSON sourcemap instance.
   * @param obj
   * @returns
   */
  public static fromObject(obj: Object) {
    return new RemappingState(obj);
  }

  /**
   * Starts sourcemap source path remapping from the specified .js.map file.
   * @param file
   * @returns
   */
  public static fromFile(file: fs.PathLike) {
    return this.fromBuffer(fs.readFileSync(file));
  }

  /**
   * Starts sourcemap source path remapping from a loaded but unparsed file.
   * @param buffer
   * @returns
   */
  public static fromBuffer(buffer: Buffer) {
    return this.fromObject(convertSourceMap.fromJSON(buffer).toObject());
  }
}