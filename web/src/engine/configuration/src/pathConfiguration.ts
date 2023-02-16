import { PathOptionSpec } from "./optionSpec.interface.js";

export default class PathConfiguration {
  readonly root: string;
  readonly resources: string;
  readonly keyboards: string;
  readonly fonts: string;
  readonly protocol: string;

  constructor(pathSpec: Required<PathOptionSpec>, sourcePath: string) {
    const addDelimiter = (p: string) => {
      // Add delimiter if missing
      if(p.substring(p.length-1, p.length) != '/') {
        return p + '/';
      } else {
        return p;
      }
    }

    sourcePath = addDelimiter(sourcePath);
    const _rootPath = sourcePath.replace(/(https?:\/\/)([^\/]*)(.*)/,'$1$2/');
    this.protocol = sourcePath.replace(/(.{3,5}:)(.*)/,'$1');

    // Local function to convert relative to absolute URLs
    // with respect to the source path, server root and protocol
    const fixPath = (p: string) => {
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
      return sourcePath + p;
    }

    // Get default paths and device options
    this.root = _rootPath;
    if(pathSpec.root != '') {
      this.root = fixPath(pathSpec.root);
    } else {
      this.root = fixPath(_rootPath);
    }

    // Resources are located with respect to the engine by default
    let resources = pathSpec.resources; // avoid mutating the parameter!
    if(resources == '') {
      resources = sourcePath;
    }

    // Convert resource, keyboard and font paths to absolute URLs
    this.resources = fixPath(resources);
    this.keyboards = fixPath(pathSpec.keyboards);
    this.fonts = fixPath(pathSpec.fonts);
  }
}