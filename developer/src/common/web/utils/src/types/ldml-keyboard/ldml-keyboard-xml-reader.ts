/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Reads a LDML XML keyboard file into JS object tree and resolves imports
 */
import { SchemaValidators, util } from '@keymanapp/common-types';
import { XMLParser } from 'fast-xml-parser';
import { CommonTypesMessages } from '../../common-messages.js';
import { CompilerCallbacks } from '../../compiler-interfaces.js';
import { LDMLKeyboardXMLSourceFile, LKImport, ImportStatus } from './ldml-keyboard-xml.js';
import { constants } from '@keymanapp/ldml-keyboard-constants';
import { LDMLKeyboardTestDataXMLSourceFile, LKTActionType, LKTStartContext, LKTTest } from './ldml-keyboard-testdata-xml.js';

import boxXmlArray = util.boxXmlArray;

interface NameAndProps  {
  '$'?: any; // content
  '#name'?: string; // element name
  '$$'?: any; // children
};

/** produced by fast-xml-parser */
interface XMLAttributeMap {
  ':@'?: Map<string, string>; // attribute values
}

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
      if (!source.keyboard3.displays) {
        source.keyboard3.displays = {
          display: [],
          displayOptions: {},
        };
      }
    }
    boxXmlArray(source?.keyboard3, 'layers');
    boxXmlArray(source?.keyboard3?.displays, 'display');
    boxXmlArray(source?.keyboard3?.keys, 'key');
    boxXmlArray(source?.keyboard3?.flicks, 'flick');
    boxXmlArray(source?.keyboard3?.locales, 'locale');
    boxXmlArray(source?.keyboard3, 'transforms');
    if(source?.keyboard3?.layers) {
      for(const layers of source?.keyboard3?.layers) {
        boxXmlArray(layers, 'layer');
        if(layers?.layer) {
          for(const layer of layers?.layer) {
            boxXmlArray(layer, 'row');
          }
        }
      }
    }
    if(source?.keyboard3?.forms?.form) {
      boxXmlArray(source?.keyboard3?.forms, 'form');
      for(const form of source?.keyboard3?.forms?.form) {
        boxXmlArray(form, 'scanCodes');
      }
    }
    if(source?.keyboard3?.flicks) {
      boxXmlArray(source?.keyboard3?.flicks, 'flick');
      for(const flick of source?.keyboard3?.flicks?.flick) {
        boxXmlArray(flick, 'flickSegment');
      }
    }
    if(source?.keyboard3?.variables) {
      boxXmlArray(source?.keyboard3?.variables, 'set');
      boxXmlArray(source?.keyboard3?.variables, 'string');
      boxXmlArray(source?.keyboard3?.variables, 'uset');
    }
    if(source?.keyboard3?.transforms) {
      for(const transforms of source.keyboard3.transforms)  {
        boxXmlArray(transforms, 'transformGroup');
        // need to see if there's an empty ('') element.
        // the schema allows an empty object, but the spec doesn't.
        for (let i=0; i<transforms.transformGroup.length; i++) {
          if (transforms.transformGroup[i] === '') {
            // substitute empty object. the compiler will complain about it.
            transforms.transformGroup[i] = { };
          }
        }
        for (const transformGroup of transforms.transformGroup) {
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
          // resolveImports has already logged a message
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
            // resolveImports has already logged a message
            return false;
          }
          // now delete the import array we so carefully constructed, the caller does not
          // want to see it.
          delete obj['import'];
        } else {
          if (!this.boxImportsAndSpecials(obj[key], key)) {
            // resolveImports has already logged a message
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
        // resolveOneImport has already logged a message
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
        // resolveOneImport has already logged a message
        return false;
      }
    } else if (subtag === 'forms') {
      // <import base="cldr" path="45/scanCodes-implied.xml"/>
      if (!this.resolveOneImport(obj, subtag, {
        base: constants.cldr_import_base,
        path: constants.cldr_implied_forms_import
      }, true)) {
        // resolveOneImport has already logged a message
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
    if(!SchemaValidators.default.ldmlKeyboard3(source)) {
      for (const err of (<any>SchemaValidators.default.ldmlKeyboard3).errors) {
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
      const parser = new XMLParser({
        ignoreAttributes: false, // We'd like attributes, please
        attributeNamePrefix: '', // to avoid '@_' prefixes
        trimValues: false, // preserve spaces, but:
        htmlEntities: true,
        tagValueProcessor: (tagName, tagValue /*, jPath, hasAttributes, isLeafNode*/) => {
          // since trimValues: false, we need to zap any element values that would be trimmed.
          // currently, the LDML spec doesn't have any element values, but this
          // future-proofs us a little in that element values are allowed, just trimmed.
          // if we do need elements in the future, we'd check the preserve-space attribute here.
          return tagValue?.trim();
        },
      });
      const a = parser.parse(new TextDecoder().decode(file), true);
      delete a['?xml']; // fast-xml-parser includes the XML prologue, it's not in the schema so we delete it.
      return a as LDMLKeyboardXMLSourceFile;
  }

  /**
   * @param file
   * @returns source on success, otherwise null
   */
  public load(file: Uint8Array): LDMLKeyboardXMLSourceFile | null {
    if (!file) {
      throw new Error(`file parameter must not be null`);
    }

    let source: LDMLKeyboardXMLSourceFile = null;
    try {
      source = this.loadUnboxed(file);
    } catch(e) {
      this.callbacks.reportMessage(CommonTypesMessages.Error_InvalidXml({e}));
      return null;
    }

    if (this.boxArrays(source)) {
      return source;
    }

    // boxArrays ... resolveImports has already logged a message
    return null;
  }

  loadTestDataUnboxed(file: Uint8Array): any {
    const parser = new XMLParser({
      htmlEntities: true,
      ignoreAttributes: false, // We'd like attributes, please
      attributeNamePrefix: '', // avoid @_
      preserveOrder: true,     // Gives us a 'special' format - see boxTestDataArrays() below
    });
    const a : any = parser.parse(file.toString())
    return a; // Why 'any'? Because we need to box up the $'s into proper properties.
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

  /** Apply the attributes to the target */
  applyTestAttributes(target: any, source: XMLAttributeMap) {
    if (!source[':@']) return;
    for (const [k, v] of Object.entries(source[':@'])) {
      if (target[k]) throw Error(`Internal error: key ${k} applied twice.`);
      target[k] = v;
    }
  }


  /**
   *
   * @param obj target object
   * @param source input data
   * @param subtag name to extract
   */
  stuffBoxes(obj: any, source: any, subtag: string) {
    // copy any attributes
    this.applyTestAttributes(obj, source); // apply top level attrs
    const v = source[subtag]; // e.g.  keyboardTest3: { … }
    if (!v) return; // no body

    // v is an array of entries.
    for (const e of v) {
      const subsubtag = (Object.keys(e).filter(n => n != ':@'))[0];
      if (!subsubtag) throw Error(`No sub-subtag of ${subtag}`);

      if (subsubtag === 'startContext') {
        // special handling on this one
        const t = obj as LKTTest;
        t.startContext = e[':@'] as LKTStartContext;
      } else if (subtag === 'test') {
        // These go under 'actions', special handling.
        const t = obj as LKTTest;
        if (t.actions === undefined) t.actions = [];
        const count = t.actions.length;
        // set the type for polymorphism
        t.actions[count] = { type: subsubtag as LKTActionType };
        // fill with attrs from the test item
        this.stuffBoxes(t.actions[count], e, subsubtag);
      } else {
        if (obj[subsubtag] === undefined) {
          // initialize the subtag. Handle a couple special cases
          if (subsubtag === 'test' || subsubtag === 'tests' || subsubtag === 'repertoire') {
            obj[subsubtag] = [];
          } else {
            obj[subsubtag] = {};
          }
        }
        // now, initialize the object
        if (Array.isArray(obj[subsubtag])) {
          const count = obj[subsubtag].length;
          // stuff into next array slot
          obj[subsubtag][count] = {};
          this.stuffBoxes(obj[subsubtag][count], e, subsubtag);
        } else {
          // overwrite object
          this.stuffBoxes(obj[subsubtag], e, subsubtag);
        }
      } // not a test action
    }
  }

  /**
   * Process the special format provided by `preserveOrder: true` option to the XML parser,
   * needed to handle the text data with variable interleaved elements.
   */
  boxTestDataArrays(raw: any) : LDMLKeyboardTestDataXMLSourceFile | null {
    if (!raw) return null;

    // It's less of a boxing and more of a reconstituting.
    // again see https://github.com/NaturalIntelligence/fast-xml-parser/blob/HEAD/docs/v4/2.XMLparseOptions.md#preserveorder

    // start with an empty document..
    const a : LDMLKeyboardTestDataXMLSourceFile = {
      keyboardTest3: {}
    };

    // skip the XML prologue
    raw = raw.filter((e: any) => !!e['keyboardTest3'])[0];

    this.stuffBoxes(a.keyboardTest3, raw, 'keyboardTest3');

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
