import * as xml2js from 'xml2js';
import { LDMLKeyboardXMLSourceFile, LKImport, ImportStatus } from './ldml-keyboard-xml.js';
import { boxXmlArray } from '../util/util.js';
import { CompilerCallbacks } from '../util/compiler-interfaces.js';
import { constants } from '@keymanapp/ldml-keyboard-constants';
import { CommonTypesMessages } from '../util/common-events.js';
import { LDMLKeyboardTestDataXMLSourceFile, LKTTest, LKTTests } from './ldml-keyboard-testdata-xml.js';
import SchemaValidators from '../schema-validators.js';

interface NameAndProps  {
  '$'?: any; // content
  '#name'?: string; // element name
  '$$'?: any; // children
};

export class LDMLKeyboardXMLSourceFileReaderOptions {
  importsPath: string;
};

export class LDMLKeyboardXMLSourceFileReader {
  constructor(private options: LDMLKeyboardXMLSourceFileReaderOptions, private callbacks : CompilerCallbacks) {
  }

  static get defaultImportsURL(): [string,string] {
    return ['../import/', import.meta.url];
  }

  readImportFile(version: string, subpath: string): Uint8Array {
    const importPath = this.callbacks.resolveFilename(this.options.importsPath, `${version}/${subpath}`);
    return this.callbacks.loadFile(importPath);
  }

  /**
   * xml2js will not place single-entry objects into arrays.
   * Easiest way to fix this is to box them ourselves as needed
   * @param source any
   * @returns true on success, false on failure
   */
  private boxArrays(source: any) : boolean {
    if (source?.keyboard3) {
      if (!source.keyboard3.keys) {
        // Note: this is here to put a substrate for the imported keys
        source.keyboard3.keys = {
          key: [],
        };
      }
      if (!source.keyboard3.keys.import) {
        source.keyboard3.keys.import = [];
      }
      if (!source.keyboard3.forms) {
        source.keyboard3.forms = {
          form: [],
        };
      }
      if (!source.keyboard3.forms.import) {
        source.keyboard3.forms.import = [];
      }
    }
    boxXmlArray(source?.keyboard3, 'layers');
    boxXmlArray(source?.keyboard3?.displays, 'display');
    boxXmlArray(source?.keyboard3?.keys, 'key');
    boxXmlArray(source?.keyboard3?.flicks, 'flick');
    boxXmlArray(source?.keyboard3?.locales, 'locale');
    boxXmlArray(source?.keyboard3, 'transforms');
    if(source?.keyboard3?.layers) {
      for(let layers of source?.keyboard3?.layers) {
        boxXmlArray(layers, 'layer');
        if(layers?.layer) {
          for(let layer of layers?.layer) {
            boxXmlArray(layer, 'row');
          }
        }
      }
    }
    if(source?.keyboard3?.forms?.form) {
      boxXmlArray(source?.keyboard3?.forms, 'form');
      for(let form of source?.keyboard3?.forms?.form) {
        boxXmlArray(form, 'scanCodes');
      }
    }
    if(source?.keyboard3?.flicks) {
      boxXmlArray(source?.keyboard3?.flicks, 'flick');
      for(let flick of source?.keyboard3?.flicks?.flick) {
        boxXmlArray(flick, 'flickSegment');
      }
    }
    if(source?.keyboard3?.variables) {
      boxXmlArray(source?.keyboard3?.variables, 'set');
      boxXmlArray(source?.keyboard3?.variables, 'string');
      boxXmlArray(source?.keyboard3?.variables, 'uset');
    }
    if(source?.keyboard3?.transforms) {
      for(let transforms of source.keyboard3.transforms)  {
        boxXmlArray(transforms, 'transformGroup');
        for (let transformGroup of transforms.transformGroup) {
          boxXmlArray(transformGroup, 'transform');
          boxXmlArray(transformGroup, 'reorder');
        }
      }
    }
    return this.boxImportsAndSpecials(source, 'keyboard3');
  }

  /**
   * Recurse over object, boxing up any specials or imports
   * @param obj any object to be traversed
   * @param subtag the leafmost enclosing tag such as 'keyboard3'
   * @returns true on success, false on failure
   */
  private boxImportsAndSpecials(obj: any, subtag: string) : boolean {
    if (!obj) return true;
    if (Array.isArray(obj)) {
      for (const sub of obj) {
        // retain the same subtag
        if (!this.boxImportsAndSpecials(sub, subtag)) {
          return false;
        }
      }
    } else if(typeof obj === 'object') {
      for (const key of Object.keys(obj)) {
        if (key === 'special') {
          boxXmlArray(obj, key);
        } else if(key === 'import') {
          // Need to 'box it up' first for processing
          boxXmlArray(obj, key);
          // Now, resolve the import
          if (!this.resolveImports(obj, subtag)) {
            return false;
          }
          // now delete the import array we so carefully constructed, the caller does not
          // want to see it.
          delete obj['import'];
        } else {
          if (!this.boxImportsAndSpecials(obj[key], key)) {
            return false;
          }
        }
      }
    }
    return true;
  }

  /**
   *
   * @param obj object to be imported into
   * @param subtag obj's element tag, e.g. `keys`
   * @returns true on success, false on failure
   */
  private resolveImports(obj: any, subtag: string) : boolean {
    // These are in reverse order, because the imports insert at the beginning of the array.
    // first, the explicit imports
    for (const asImport of ([...obj['import'] as LKImport[]].reverse())) {
      if (!this.resolveOneImport(obj, subtag, asImport)) {
        return false;
      }
    }
    // then, the implied imports
    if (subtag === 'keys') {
      // <import base="cldr" path="45/keys-Latn-implied.xml"/>
      if (!this.resolveOneImport(obj, subtag, {
        base: constants.cldr_import_base,
        path: constants.cldr_implied_keys_import
      }, true)) {
        return false;
      }
    } else if (subtag === 'forms') {
      // <import base="cldr" path="45/scanCodes-implied.xml"/>
      if (!this.resolveOneImport(obj, subtag, {
        base: constants.cldr_import_base,
        path: constants.cldr_implied_forms_import
      }, true)) {
        return false;
      }
    }
    return true;
  }

  /**
   * @param obj the object being imported into
   * @param subtag obj's element tag, e.g. `keys`
   * @param asImport the import structure
   * @param implied true if it is an implied import
   * @returns true on success, false on failure
   */
  private resolveOneImport(obj: any, subtag: string, asImport: LKImport, implied? : boolean) : boolean {
    const { base, path } = asImport;
    if (base !== constants.cldr_import_base) {
      this.callbacks.reportMessage(CommonTypesMessages.Error_ImportInvalidBase({base, path, subtag}));
      return false;
    }
    const paths = path.split('/');
    if (paths[0] == '' || paths[1] == '' || paths.length !== 2) {
      this.callbacks.reportMessage(CommonTypesMessages.Error_ImportInvalidPath({base, path, subtag}));
      return false;
    }
    const importData: Uint8Array = this.readImportFile(paths[0], paths[1]);
    if (!importData || !importData.length) {
      this.callbacks.reportMessage(CommonTypesMessages.Error_ImportReadFail({base, path, subtag}));
      return false;
    }
    const importXml: any = this.loadUnboxed(importData); // TODO-LDML: have to load as any because it is an arbitrary part
    const importRootNode = importXml[subtag]; // e.g. <keys/>

    // importXml will have one property: the root element.
    if (!importRootNode) {
      this.callbacks.reportMessage(CommonTypesMessages.Error_ImportWrongRoot({base, path, subtag}));
      return false;
    }
    // pull all children of importXml[subtag] into obj
    for (const subsubtag of Object.keys(importRootNode).reverse()) { // e.g. <key/>
      const subsubval = importRootNode[subsubtag];
      const basePath = `${base}/${path}`;
      if (!Array.isArray(subsubval)) {
        // This is somewhat of an internal error, indicating that a non-mergeable XML file was imported
        // Not exercisable with the standard LDML imports.
        this.callbacks.reportMessage(CommonTypesMessages.Error_ImportMergeFail({base, path, subtag, subsubtag}));
        return false;
      }
      // Mark all children as an import
      subsubval.forEach(o => o[ImportStatus.import] = basePath);
      if (implied) {
        // mark all children as an implied import
        subsubval.forEach(o => o[ImportStatus.impliedImport] = basePath);
      }

      if (!obj[subsubtag]) {
        obj[subsubtag] = []; // start with empty array
      }
      obj[subsubtag] = [...subsubval, ...obj[subsubtag]];
    }
    return true;
  }

  /**
   * @returns true if valid, false if invalid
   */
  public validate(source: LDMLKeyboardXMLSourceFile | LDMLKeyboardTestDataXMLSourceFile): boolean {
    if(!SchemaValidators.ldmlKeyboard3(source)) {
      for (let err of (<any>SchemaValidators.ldmlKeyboard3).errors) {
        this.callbacks.reportMessage(CommonTypesMessages.Error_SchemaValidationError({
          instancePath: err.instancePath,
          keyword: err.keyword,
          message: err.message || 'Unknown AJV Error', // docs say 'message' is optional if 'messages:false' in options
          params: Object.entries(err.params || {}).sort().map(([k,v])=>`${k}="${v}"`).join(' '),
        }));
      }
      return false;
    }
    return true;
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
      parser.parseString(file, (e: unknown, r: unknown) => { a = r as LDMLKeyboardXMLSourceFile }); // TODO-LDML: isn't 'e' the error?
      return a;
    })();
    return source;
  }

  /**
   * @param file
   * @returns source on success, otherwise null
   */
  public load(file: Uint8Array): LDMLKeyboardXMLSourceFile | null {
    if (!file) {
      return null;
    }
    const source = this.loadUnboxed(file);
    if(this.boxArrays(source)) {
      return source;
    } else {
      return null;
    }
  }

  loadTestDataUnboxed(file: Uint8Array): any {
    let source = (() => {
      let a: any;
      let parser = new xml2js.Parser({
        // explicitArray: false,
        preserveChildrenOrder:true, // needed for test data
        explicitChildren: true, // needed for test data
        // mergeAttrs: true,
        // includeWhiteChars: false,
        // emptyTag: {} as any
        // Why "as any"? xml2js is broken:
        // https://github.com/Leonidas-from-XIV/node-xml2js/issues/648 means
        // that an old version of `emptyTag` is used which doesn't support
        // functions, but DefinitelyTyped is requiring use of function or a
        // string. See also notes at
        // https://github.com/DefinitelyTyped/DefinitelyTyped/pull/59259#issuecomment-1254405470
        // An alternative fix would be to pull xml2js directly from github
        // rather than using the version tagged on npmjs.com.
      });
      parser.parseString(file, (e: unknown, r: unknown) => { a = r as any }); // TODO-LDML: isn't 'e' the error?
      return a; // Why 'any'? Because we need to box up the $'s into proper properties.
    })();
    return source;
  }

  /**
   * Filter the obj array for a subtag
   * @param source array of source objs
   * @param subtag subtag to filter on
   * @returns
   */
  findSubtagArray(source: NameAndProps[], subtag: string): NameAndProps[]  {
    return source?.filter(o => o['#name'] === subtag);
  }

  /**
   * Get exactly one element
   * @param source
   * @param subtag
   * @returns
   */
  findSubtag(source: NameAndProps[], subtag: string): NameAndProps | null {
    const r = this.findSubtagArray(source, subtag);
    if (!r || r.length === 0) {
      return null;
    } else if (r.length === 1) {
      return r[0];
    } else {
      this.callbacks.reportMessage(CommonTypesMessages.Error_TestDataUnexpectedArray({subtag}));
      return null; // ERROR
    }
  }

  /**
   * The default test data stuffer.
   * Just gets $ (the attrs) as the body.
   * Override to use something more complex, such as including child nodes.
   * @param o object to map
   * @param r back ref to reader
   */
  static readonly defaultMapper = ((o : NameAndProps, r: LDMLKeyboardXMLSourceFileReader) => o?.$);

  /**
   *
   * @param obj target object
   * @param source array of $/#name strings
   * @param subtag name to extract
   * @param mapper custom mapper function
   */
  stuffBoxes(obj: any, source: NameAndProps[], subtag: string, asArray?: boolean, mapper?: (v: NameAndProps, r: LDMLKeyboardXMLSourceFileReader) => any) {
    if (!mapper) {
      mapper = LDMLKeyboardXMLSourceFileReader.defaultMapper;
    }
    if (asArray) {
      const r = this;
      obj[subtag] = this.findSubtagArray(source, subtag)?.map((v) => mapper(v, r)); // extract contents only
    } else {
      obj[subtag] = mapper(this.findSubtag(source, subtag), this); // run the mapper once
    }
  }

  boxTestDataArrays(raw: any) : LDMLKeyboardTestDataXMLSourceFile | null {
    if (!raw) return null;
    const a : LDMLKeyboardTestDataXMLSourceFile = {
      keyboardTest3: {
        conformsTo: raw?.keyboardTest3?.$?.conformsTo,
      }
    };

    const $$ : NameAndProps[] = raw?.keyboardTest3?.$$;

    this.stuffBoxes(a.keyboardTest3, $$, 'info');
    this.stuffBoxes(a.keyboardTest3, $$, 'repertoire', true);
    this.stuffBoxes(a.keyboardTest3, $$, 'tests', true, (o, r) => {
      // start with basic unpack
      const tests : LKTTests = LDMLKeyboardXMLSourceFileReader.defaultMapper(o, r);
      // add ingredients
      r.stuffBoxes(tests, o.$$, 'test', true, (o, r) => {
        // start with basic unpack
        const test : LKTTest = LDMLKeyboardXMLSourceFileReader.defaultMapper(o, r);
        // add ingredients
        const $$ : NameAndProps[] = o.$$;
        r.stuffBoxes(test, $$, 'startContext'); // singleton
        // now the actions
        test.actions = $$.map(v => {
          const type = v['#name']; // element name
          if (type === 'startContext') {
            return null; // handled above
          }
          const subv = LDMLKeyboardXMLSourceFileReader.defaultMapper(v, r);
          return Object.assign({ type }, subv);
        }).filter(v => v !== null);
        return test;
      });
      return tests;
    });

    return a;
  }

  /**
   * @param file test file
   * @returns source on success, otherwise null
   */
  public loadTestData(file: Uint8Array): LDMLKeyboardTestDataXMLSourceFile | null {
    if (!file) {
      return null;
    }
    const source = this.loadTestDataUnboxed(file);
    return this.boxTestDataArrays(source);
  }
}
