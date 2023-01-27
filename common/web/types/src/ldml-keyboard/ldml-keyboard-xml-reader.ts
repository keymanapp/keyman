import * as xml2js from 'xml2js';
import { LDMLKeyboardXMLSourceFile, LKImport } from './ldml-keyboard-xml.js';
import Ajv from 'ajv';
import { boxXmlArray } from '../util/util.js';
import { CompilerCallbacks } from 'src/util/compiler.js';
import { constants } from '@keymanapp/ldml-keyboard-constants';

export default class LDMLKeyboardXMLSourceFileReader {
  callbacks: CompilerCallbacks;

  constructor(callbacks : CompilerCallbacks) {
    this.callbacks = callbacks;
  }

  readImportFile(version: string, subpath: string): Buffer {
    // TODO-LDML: sanitize input string
    let importPath = new URL(`../import/${version}/${subpath}`, import.meta.url);
    // TODO-LDML: support baseFileName?
    return this.callbacks.loadFile(importPath.pathname, importPath);
  }

  /**
   * xml2js will not place single-entry objects into arrays.
   * Easiest way to fix this is to box them ourselves as needed
   * @param source any
   */
  private boxArrays(source: any) {
    if (source?.keyboard) {
      if (!source.keyboard.keys) {
        source.keyboard.keys = {
          key: [],
          flicks: [],
        };
      }
      if (!source.keyboard.keys?.import) {
        source.keyboard.keys.import = [];
      }
    }
    boxXmlArray(source?.keyboard, 'layers');
    boxXmlArray(source?.keyboard?.displays, 'display');
    boxXmlArray(source?.keyboard?.names, 'name');
    boxXmlArray(source?.keyboard?.vkeys, 'vkey');
    boxXmlArray(source?.keyboard?.keys, 'key');
    boxXmlArray(source?.keyboard?.keys, 'flicks');
    boxXmlArray(source?.keyboard?.locales, 'locale');
    boxXmlArray(source?.keyboard, 'transforms');
    if(source?.keyboard?.layers) {
      for(let layers of source?.keyboard?.layers) {
        boxXmlArray(layers, 'layer');
        if(layers?.layer) {
          for(let layer of layers?.layer) {
            boxXmlArray(layer, 'row');
          }
        }
      }
    }
    if(source?.keyboard?.keys?.flicks) {
      for(let flicks of source?.keyboard?.keys?.flicks) {
        boxXmlArray(flicks, 'flick');
      }
    }
    if(source?.keyboard?.transforms) {
      for(let transform of source.keyboard.transforms)  {
        boxXmlArray(transform, 'transform');
      }
    }
    boxXmlArray(source?.keyboard?.reorders, 'reorder');
    this.boxImportsAndSpecials(source, 'keyboard');
    return source;
  }

  /**
   * Recurse over object, boxing up any specials or imports
   * @param obj any object to be traversed
   * @param subtag the leafmost enclosing tag such as 'keyboard'
   */
  private boxImportsAndSpecials(obj: any, subtag: string) {
    if (!obj) return;
    if (Array.isArray(obj)) {
      for (const sub of obj) {
        // retain the same subtag
        this.boxImportsAndSpecials(sub, subtag);
      }
    } else if(typeof obj === 'object') {
      for (const key of Object.keys(obj)) {
        if (key === 'special') {
          boxXmlArray(obj, key);
        } else if(key === 'import') {
          // Need to 'box it up' first for processing
          boxXmlArray(obj, key);
          // Now, resolve the import
          this.resolveImports(obj, subtag);
          // now delete the import array we so carefully constructed, the caller does not
          // want to see it.
          delete obj['import'];
        } else {
          this.boxImportsAndSpecials(obj[key], key);
        }
      }
    }
  }

  private resolveImports(obj: any, subtag: string) {
    // first, the implied imports
    if (subtag === 'keys') {
      // <import base="cldr" path="techpreview/keys-Latn-implied.xml"/>
      this.resolveOneImport(obj, subtag, {
        base: constants.cldr_import_base,
        path: constants.cldr_implied_keys_import
      });
    }

    // now, the explicit imports
    for (const asImport of (obj['import'] as LKImport[])) {
      this.resolveOneImport(obj, subtag, asImport);
    }
  }

  private resolveOneImport(obj: any, subtag: string, asImport: LKImport) {
    if (asImport.base !== constants.cldr_import_base) {
      throw new Error(`import element with base ${asImport.base} is unsupported, only ${constants.cldr_import_base} is supported.`);
    }
    const paths = asImport.path.split('/');
    if (!paths[0] || !paths[1] || paths.length !== 2) {
      throw new Error(`import element with invalid path ${asImport.path}: expect the form '${constants.cldr_version_latest}./*.xml'`);
    }
    const importData: Uint8Array = this.readImportFile(paths[0], paths[1]);
    if (!importData || !importData.length) {
      throw new Error(`could not read data with path ${asImport.path}: expect the form '${constants.cldr_version_latest}./*.xml'`);
    }
    const importXml: any = this.loadUnboxed(importData); // TODO-LDML: have to load as any because it is an arbitrary part
    const importRootNode = importXml[subtag]; // e.g. <keys/>

    // importXml will have one property: the root element.
    if (!importRootNode) {
      throw new Error(`Invalid import file ${asImport.path}: expected ${subtag} as root element`);
    }
    // pull all children of importXml[subtag] into obj
    for (const subsubtag of Object.keys(importRootNode)) { // e.g. <key/>
      const subsubval = importRootNode[subsubtag];
      if (!Array.isArray(subsubval)) {
        throw new Error(`Problem importing ${asImport.path}: not sure how to handle non-array ${subtag}.${subsubtag}`);
      }
      if (!obj[subsubtag]) {
        obj[subsubtag] = []; // start with empty array
      }
      obj[subsubtag] = [...subsubval, ...obj[subsubtag]];
    }
  }

  public validate(source: LDMLKeyboardXMLSourceFile, schemaSource: Buffer): void {
    const schema = JSON.parse(schemaSource.toString('utf8'));
    const ajv = new Ajv();
    if(!ajv.validate(schema, source)) {
      // Try to improve the message
      if (ajv.errors?.length === 1) {
        // Only one error. Try to improve the message.
        const err = ajv.errors[0];
        const { instancePath, keyword, params, message } = err;
        throw new Error(`${instancePath}: ${keyword}: ${message} ${JSON.stringify(params||{})}`);
      } else {
        // Not a single error, so fall through to errorsText()
        throw new Error(ajv.errorsText());
      }
    }
  }

  loadUnboxed(file: Uint8Array): LDMLKeyboardXMLSourceFile {
    let source = (() => {
      let a: LDMLKeyboardXMLSourceFile;
      let parser = new xml2js.Parser({
        explicitArray: false,
        mergeAttrs: true,
        includeWhiteChars: false,
        emptyTag: {} as any
        // Why "as any"? xml2js is broken:
        // https://github.com/Leonidas-from-XIV/node-xml2js/issues/648 means
        // that an old version of `emptyTag` is used which doesn't support
        // functions, but DefinitelyTyped is requiring use of function or a
        // string. See also notes at
        // https://github.com/DefinitelyTyped/DefinitelyTyped/pull/59259#issuecomment-1254405470
        // An alternative fix would be to pull xml2js directly from github
        // rather than using the version tagged on npmjs.com.
      });
      parser.parseString(file, (e: unknown, r: unknown) => { a = r as LDMLKeyboardXMLSourceFile });
      return a;
    })();
    return source;
  }

  public load(file: Uint8Array): LDMLKeyboardXMLSourceFile {
    const source = this.loadUnboxed(file);
    return this.boxArrays(source);
  }
}
