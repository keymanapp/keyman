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

export class PathConfiguration implements OSKResourcePathConfiguration {
  private readonly sourcePath: string;
  private _root: string;
  private _resources: string;
  private _keyboards: string;

  // May get its initial value from the Keyman Cloud API after a query if not
  // otherwise specified.
  private _fonts: string;
  readonly protocol: string;

  constructor(pathSpec: Required<PathOptionSpec>, sourcePath: string) {
    const sourceURL = new URL(sourcePath);

    sourcePath = addDelimiter(sourcePath);
    this.sourcePath = sourcePath;
    this.protocol = sourceURL.protocol;

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

    // Absolute - with protocol specified
    const protocolList = [
      'http:',
      'https:',
      'file:',
      // If using a custom origin (say, hosted in an iOS WebView via WKURLSchemeHandler)
      this.protocol
    ];

    if(protocolList.find((protocol) => p.startsWith(protocol))) {
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