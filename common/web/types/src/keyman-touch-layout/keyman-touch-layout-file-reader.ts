import { default as AjvModule } from 'ajv';
const Ajv = AjvModule.default; // The actual expected Ajv type.
import { TouchLayoutFile } from "./keyman-touch-layout-file.js";
import Schemas from '../../src/schemas.js';

export class TouchLayoutFileReader {
  public read(source: Uint8Array): TouchLayoutFile {
    const decoder = new TextDecoder('utf-8', {fatal:true, ignoreBOM: true});
    let sourceString: string;
    try {
      sourceString = decoder.decode(source);
    /* c8 ignore next 7 */
    } catch(e) {
      if(e instanceof TypeError) {
        // TODO: Do we want to do something else with this?
        throw e;
      }
      throw e;
    }

    let result: TouchLayoutFile;
    try {
      result = JSON.parse(sourceString, function(key, value) {
        // `row.id` should be number, but may have been stringified; we use
        // presence of `key` property to recognise this as a `TouchLayoutRow`.
        if(this.key && key == 'id' && typeof value == 'string') {
          let newValue = parseInt(value, 10);
          /* c8 ignore next 3 */
          if(isNaN(newValue)) {
            throw new TypeError(`Invalid row.id: "${value}"`);
          }
          return newValue;
        }

        // `key.width`, `key.pad`, `key.sp` should be number, but may have been
        // stringified
        if(key == 'width' || key == 'pad' || key == 'sp') {
          if(value === '') {
            // Empty string is equivalent to not present, so fall back to
            // default value
            return undefined;
          }

          let newValue = parseInt(value, 10);
          /* c8 ignore next 3 */
          if(isNaN(newValue)) {
            throw new TypeError(`Invalid [sub]key.${key}: "${value}"`);
          }
          return newValue;
        }

        if(Array.isArray(value) && value.length == 0) {
          // Delete empty arrays
          return undefined;
        }

        return value;
      });
    /* c8 ignore next 7 */
    } catch(e) {
      if(e instanceof SyntaxError) {
        // TODO: Do we want to do something else with this?
        throw e;
      }
      throw e;
    }

    return result;
  }

  public validate(source: TouchLayoutFile): void {
    const ajv = new Ajv();
    if(!ajv.validate(Schemas.touchLayoutClean, source))
    /* c8 ignore next 3 */
    {
      throw new Error(ajv.errorsText());
    }
  }


};