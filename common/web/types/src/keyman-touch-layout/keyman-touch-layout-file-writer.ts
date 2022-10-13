import { TouchLayoutFile, TouchLayoutPlatform, TouchLayoutKey, TouchLayoutSubKey } from "./keyman-touch-layout-file.js";

export interface TouchLayoutFileWriterOptions {
  formatted?: boolean;
};

export class TouchLayoutFileWriter {

  options: TouchLayoutFileWriterOptions;

  constructor(options?: TouchLayoutFileWriterOptions) {
    this.options = {...options};
  }

  /**
   * Writes the touch layout to a .keyman-touch-layout JSON file
   * @param source TouchLayoutFile
   * @returns Uint8Array, the .keyman-touch-layout file
   */
  write(source: TouchLayoutFile): Uint8Array {
    const output = JSON.stringify(source, null, this.options?.formatted ? 2 : undefined);
    const encoder = new TextEncoder();
    return encoder.encode(output);
  }

  /**
   * Compiles the touch layout file into a KeymanWeb-compatible JSON-style
   * object string. In the future, this may be optimized to remove unnecessary
   * quoting of property names, and remove unused properties, as this string
   * is embedded into .js code.
   * @param source
   * @returns string
   */
  compile(source: TouchLayoutFile): string {
    // Deep copy the source
    source = JSON.parse(JSON.stringify(source));

    // Fixup pad, width and sp to string types, as that's what KeymanWeb
    // currently expects

    const fixupKey = (key: TouchLayoutKey | TouchLayoutSubKey) => {
      if(Object.hasOwn(key, 'pad')) (key.pad as any) = key.pad.toString();
      if(Object.hasOwn(key, 'sp')) (key.sp as any) = key.sp.toString();
      if(Object.hasOwn(key, 'width')) (key.width as any) = key.width.toString();
    };

    const fixupPlatform = (platform: TouchLayoutPlatform) => {
      for(let layer of platform.layer) {
        for(let row of layer.row) {
          for(let key of row.key) {
            fixupKey(key);
            if(key.sk) {
              for(let sk of key.sk) {
                fixupKey(sk);
              }
            }
            if(key.multitap) {
              for(let sk of key.multitap) {
                fixupKey(sk);
              }
            }
            if(key.flick) {
              for(let id of Object.keys(key.flick)) {
                fixupKey((key.flick as any)[id] as TouchLayoutSubKey);
              }
            }
          }
        }
      }
    };

    if(source.desktop) {
      fixupPlatform(source.desktop);
    }
    if(source.phone) {
      fixupPlatform(source.phone);
    }
    if(source.tablet) {
      fixupPlatform(source.tablet);
    }

    return JSON.stringify(source, null, this.options?.formatted ? 2 : undefined);
  }
};