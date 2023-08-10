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
  public write(source: TouchLayoutFile): Uint8Array {
    const output = this.toJSONString(source);
    const encoder = new TextEncoder();
    return encoder.encode(output);
  }

  /**
   * Gets the output as a JSON string
   */
  public toJSONString(source: TouchLayoutFile): string {
    return JSON.stringify(source, null, this.options?.formatted ? 2 : undefined);
  }

  /**
   * Compiles the touch layout file into a KeymanWeb-compatible JSON-style
   * object string. In the future, this may be optimized to remove unnecessary
   * quoting of property names, and remove unused properties, as this string
   * is embedded into .js code.
   * @param source
   * @returns string
   */
  public compile(source: TouchLayoutFile): string {
    return this.toJSONString(this.fixup(source));
  }

  public fixup(source: TouchLayoutFile): TouchLayoutFile {
    // Deep copy the source
    source = JSON.parse(JSON.stringify(source));

    // Fixup pad, width and sp to string types, as that's what KeymanWeb
    // currently expects

    const fixupKey = (key: TouchLayoutKey | TouchLayoutSubKey) => {
      if(Object.hasOwn(key, 'pad')) {
        if(key.pad == 0) {
          delete key.pad;
        }
        else {
          (key.pad as any) = key.pad.toString();
        }
      }
      if(Object.hasOwn(key, 'sp')) {
        if(key.sp == 0) {
          delete key.sp;
        }
        else {
          (key.sp as any) = key.sp.toString();
        }
      }
      if(Object.hasOwn(key, 'width')) {
        if(key.width == 0) {
          delete key.width;
        }
        else {
          (key.width as any) = key.width.toString();
        }
      }
      if(Object.hasOwn(key, 'text') && key.text === '') delete key.text;
      if(Object.hasOwn(key, 'id') && <string>key.id === '') delete key.id;
      if(Object.hasOwn(key, 'hint') && (<any>key).hint === '') delete (<any>key).hint;
      if(Object.hasOwn(key, 'default') && (<any>key).default === false) delete (<any>key).default;
    };

    const fixupPlatform = (platform: TouchLayoutPlatform) => {
      // font and fontsize are eliminated if nullish:
      if(Object.hasOwn(platform,'font') && platform.font == '') delete platform.font;
      if(Object.hasOwn(platform,'fontsize') && platform.fontsize == '') delete platform.fontsize;
      // displayUnderlying is always written out by kmcomp, so we do the same for kmc:
      platform.displayUnderlying = !!platform.displayUnderlying;

      for(let layer of platform.layer) {
        for(let row of layer.row) {
          // this matches the old spec for touch layout files
          (row.id as any) = row.id.toString();
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

    return source;
  }
};