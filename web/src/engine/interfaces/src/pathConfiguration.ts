import { PathOptionSpec } from "./optionSpec.interface.js";
import { OSKResourcePathConfiguration } from './oskResourcePathConfiguration.interface.js';

const addDelimiter = (p: string) => {
  // Add delimiter if missing
  if(p.substring(p.length-1) != '/') {
    return p + '/';
  } else {
    return p;
  }
}

export default class PathConfiguration implements OSKResourcePathConfiguration {
  private readonly sourcePath: string;
  private _root: string;
  private _resources: string;
  private _keyboards: string;

  // May get its initial value from the Keyman Cloud API after a query if not
  // otherwise specified.
  private _fonts: string;
  readonly protocol: string;

  /*
   * Pre-modularization code corresponding to `sourcePath`:
  ```
   // Determine path and protocol of executing script, setting them as
   // construction defaults.
   //
   // This can only be done during load when the active script will be the
   // last script loaded.  Otherwise the script must be identified by name.

   var scripts = document.getElementsByTagName('script');
   var ss = scripts[scripts.length-1].src;
   var sPath = ss.substr(0,ss.lastIndexOf('/')+1);
   ```
   */
  constructor(pathSpec: Required<PathOptionSpec>, sourcePath: string) {
    sourcePath = addDelimiter(sourcePath);
    this.sourcePath = sourcePath;
    this.protocol = sourcePath.replace(/(.{3,5}:)(.*)/,'$1');

    this.updateFromOptions(pathSpec);
  }

  updateFromOptions(pathSpec: Required<PathOptionSpec>) {
    const _rootPath = this.sourcePath.replace(/(https?:\/\/)([^\/]*)(.*)/,'$1$2/');

    // Get default paths and device options
    this._root = _rootPath;
    if(pathSpec.root != '') {
      this._root = this.fixPath(pathSpec.root);
    } else {
      this._root = this.fixPath(_rootPath);
    }

    // Resources are located with respect to the engine by default
    let resources = pathSpec.resources; // avoid mutating the parameter!
    if(resources == '') {
      resources = this.sourcePath;
    }

    // Convert resource, keyboard and font paths to absolute URLs
    this._resources = this.fixPath(resources);
    this._keyboards = this.fixPath(pathSpec.keyboards);
    this._fonts = this.fixPath(pathSpec.fonts);
  }

  // Local function to convert relative to absolute URLs
  // with respect to the source path, server root and protocol
  fixPath(p: string) {
    if(p.length == 0) {
      return p;
    }

    p = addDelimiter(p);

    // Absolute
    if((p.replace(/^(http)s?:.*/,'$1') == 'http') || (p.replace(/^(file):.*/,'$1') == 'file')) {
      return p;
    }

    // Absolute (except for protocol)
    if(p.substring(0,2) == '//') {
      return this.protocol + p;
    }

    // Relative to server root
    if(p.substring(0,1) == '/') {
      return this.root + p.substring(1);
    }

    // Otherwise, assume relative to source path
    return this.sourcePath + p;
  }

  get fonts(): string {
    return this._fonts;
  }

  updateFontPath(path: string) {
    this._fonts = this.fixPath(path);
  }

  get root(): string {
    return this._root;
  }

  get basePath(): string {
    return this.sourcePath;
  }

  get resources(): string {
    return this._resources;
  }

  get keyboards(): string {
    return this._keyboards;
  }
}