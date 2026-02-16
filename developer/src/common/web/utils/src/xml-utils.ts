/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by srl on 2024-09-27
 *
 * Abstraction for XML reading and writing
 */

import { XMLParser, XMLBuilder, XMLMetaData, X2jOptions, XmlBuilderOptions } from 'fast-xml-parser';
import { SymbolUtils } from "./symbol-utils.js";

/** Symbol giving the start offset, in chars, of the node */
const XML_META_DATA_SYMBOL = XMLParser.getMetaDataSymbol();
/** Symbol giving an override which file a node came from */
export const XML_FILENAME_SYMBOL = Symbol("XML Filename");

export type KeymanXMLType =
  'keyboard3'             // LDML <keyboard3>
  | 'keyboardTest3'       // LDML <keyboardTest3>
  | 'keylayout'           // keylayout
  | 'kps'                 // <Package>
  | 'kvks'                // <visualkeyboard>
  | 'kpj'                 // <KeymanDeveloperProject>
  ;

/** Bag of options, maximally one for each KeymanXMLType */
type KeymanXMLParserOptionsBag = {
  [key in KeymanXMLType]?: X2jOptions;
};

const PARSER_COMMON_OPTIONS: X2jOptions = {
  attributeNamePrefix: '$', // causes remapping into $: { … } objects
  htmlEntities: true,
  ignoreAttributes: false,
  ignorePiTags: true,
  numberParseOptions: { // TODO: query is this option really necessary?
    eNotation: null,
    hex: null,
    leadingZeros: null,
    skipLike: /(?:)/, // parse numbers as strings
  },
  textNodeName: '_',
};

/** map of options for the XML parser */
const PARSER_OPTIONS: KeymanXMLParserOptionsBag = {
  'keyboard3': {
    attributeNamePrefix: '@__', // We'll use this to convert attributes to strings and subobjects to arrays, when empty.
    htmlEntities: true,
    ignoreAttributes: false, // We'd like attributes, please
    tagValueProcessor: (_tagName: string, tagValue: string /*, jPath, hasAttributes, isLeafNode*/) => {
      // since trimValues: false, we need to zap any element values that would be trimmed.
      // currently, the LDML spec doesn't have any element values, but this
      // future-proofs us a little in that element values are allowed, just trimmed.
      // if we do need elements in the future, we'd check the preserve-space attribute here.
      return tagValue?.trim();
    },
    captureMetaData: true,
    trimValues: false, // preserve spaces, but see tagValueProcessor
  },
  'keyboardTest3': {
    attributeNamePrefix: '', // avoid @_
    htmlEntities: true,
    ignoreAttributes: false, // We'd like attributes, please
    ignorePiTags: true,
    preserveOrder: true,     // Gives us a 'special' format
  },
  'keylayout': {
    attributeNamePrefix: '@_', // avoid @_
    htmlEntities: true,
    ignoreAttributes: false, // We'd like attributes, please
    tagValueProcessor: (_tagName: string, tagValue: string /*, jPath, hasAttributes, isLeafNode*/) => {
      // since trimValues: false, we need to zap any element values that would be trimmed.
      return tagValue?.trim();
    },
    trimValues: false, // preserve spaces, but see tagValueProcessor
  },
  'kps': {
    ...PARSER_COMMON_OPTIONS,
  },
  'kpj': {
    ...PARSER_COMMON_OPTIONS,
    attributeNamePrefix: '', // to avoid '@_' prefixes
  },
  'kvks': {
    ...PARSER_COMMON_OPTIONS,
    tagValueProcessor: (_tagName: string, tagValue: string, _jPath: string, _hasAttributes: boolean, isLeafNode: boolean) : string | undefined => {
      if (!isLeafNode) {
        return tagValue?.trim(); // trimmed value
      } else {
        return null;  // no change to leaf nodes
      }
    },
    trimValues: false, // preserve spaces
  },
};

type KeymanXMLGeneratorOptionsBag = {
  [key in KeymanXMLType]?: XmlBuilderOptions
};

const GENERATOR_COMMON_OPTIONS: XmlBuilderOptions = {
  attributeNamePrefix: '$',
  ignoreAttributes: false,
  format: true,
  textNodeName: '_',
  suppressEmptyNode: true,
};

const GENERATOR_OPTIONS: KeymanXMLGeneratorOptionsBag = {
  kvks: {
    ...GENERATOR_COMMON_OPTIONS,
  },
  kpj: {
    ...GENERATOR_COMMON_OPTIONS,
  },
  kps: {
    ...GENERATOR_COMMON_OPTIONS,
  },
  keyboard3: {
    attributeNamePrefix: '$',
    ignoreAttributes: false,
    format: true,
    textNodeName: '_',
    suppressEmptyNode: true,
  },
};

export interface KeymanXMLMetadata extends XMLMetaData {
  /** override of name of XML file */
  [XML_FILENAME_SYMBOL]?: string;
}

/** wrapper for XML parsing support */
export class KeymanXMLReader {
  public constructor(public type: KeymanXMLType) {
  }

  /** Get metadata on a node if not already set */
  static getMetaData(o: any) : KeymanXMLMetadata {
    if(!o) return o;
    const metadata : KeymanXMLMetadata = o[XML_META_DATA_SYMBOL as any];
    return metadata;
  }

  /** Set metadata if not already set */
  public static setMetaData(o: any, metadata: KeymanXMLMetadata) : KeymanXMLMetadata {
    let m : KeymanXMLMetadata = KeymanXMLReader.getMetaData(o);
    if (!m) {
      m = {};
    }
    // copy non-symbols
    m = {...metadata, ...m};
    // copy symbols
    SymbolUtils.copySymbols(m, metadata);
    o[XML_META_DATA_SYMBOL as any] = m;
    return m;
  }

  /** set metadata on this and children with the default filename - if not already set */
  public static setDefaultFilename(data: any, filename: string) {
    if (!data || !filename) return;
    if (typeof data === 'object') {
      const m = KeymanXMLReader.getMetaData(data) || {};
      if (!m[XML_FILENAME_SYMBOL]) {
        (m as any)[XML_FILENAME_SYMBOL] = filename;
        KeymanXMLReader.setMetaData(data, m);
      }
      if (Array.isArray(data)) {
        data.forEach(e => KeymanXMLReader.setDefaultFilename(e, filename));
      } else for(const k of Object.keys(data)) {
        KeymanXMLReader.setDefaultFilename(data[k], filename);
      }
    }
  }

  /** move `{ $abc: 4 }` into `{ $: { abc: 4 } }` */
  private static fixupDollarAttributes(data: any) : any {
    if (typeof data === 'object') {
      if (Array.isArray(data)) {
        return data.map(v => KeymanXMLReader.fixupDollarAttributes(v));
      }
      // object
      const e : any = [];
      const attrs : any = [];
      Object.entries(data).forEach(([k, v]) => {
        if (k[0] === '$') {
          k = k.slice(1);
          attrs.push([k, KeymanXMLReader.fixupDollarAttributes(v)]);
        } else {
          e.push([k, KeymanXMLReader.fixupDollarAttributes(v)]);
        }
      });
      if (attrs.length) {
        e.push(['$', SymbolUtils.copySymbols(Object.fromEntries(attrs), data)]);
      }
      return SymbolUtils.copySymbols(Object.fromEntries(e), data);
    } else {
      return data;
    }
  }

  /**
   * Requires attribute prefix @__ (double underscore)
   * For attributes, just remove @__ and continue.
   * For objects, replace any empty string "" with an empty object {} */
  private static fixupEmptyStringToEmptyObject(data: any) : any {
    if (typeof data === 'object') {
      // For arrays of objects, we map "" to {}
      // "" means an empty object
      if (Array.isArray(data)) {
        return data.map(v => {
          if (v === '') {
            return {};
          } else {
            return KeymanXMLReader.fixupEmptyStringToEmptyObject(v);
          }
        });
      }
      // otherwise: remove @__ for attributes, remap objects
      const e: any = [];
      Object.entries(data).forEach(([k, v]) => {
        if (k.startsWith('@__')) {
          e.push([k.substring(3), KeymanXMLReader.fixupEmptyStringToEmptyObject(v)]);
        } else {
          if (v === '') {
            e.push([k, {}]);
          } else {
            e.push([k, KeymanXMLReader.fixupEmptyStringToEmptyObject(v)]);
          }
        }
      });
      return SymbolUtils.copySymbols(Object.fromEntries(e), data);
    } else {
      return data;
    }
  }


  /**
   * Requires attribute prefix @_ (double underscore)
   * For attributes, just remove @_ and continue.
   * For objects, replace any empty string "" with an empty object {} */
  private static fixupEmptyStringToEmptyObject_keylayout(data: any) : any {

    if (typeof data === 'object') {
      // For arrays of objects, we map "" to {}
      // "" means an empty object
      if (Array.isArray(data)) {
        return data.map(v => {
          if (v === '') {
            return {};
          } else {
            return KeymanXMLReader.fixupEmptyStringToEmptyObject_keylayout(v);
          }
        });
      }
      // otherwise: remove @_ for attributes, remap objects
      const e: any = [];
      Object.entries(data).forEach(([k, v]) => {
        if (k.startsWith('@_')) {
          e.push([k.substring(3), KeymanXMLReader.fixupEmptyStringToEmptyObject_keylayout(v)]);
        } else {
          if (v === '') {
            e.push([k, {}]);
          } else {
            e.push([k, KeymanXMLReader.fixupEmptyStringToEmptyObject_keylayout(v)]);
          }
        }
     });
      return Object.fromEntries(e);
    } else {
      return data;
    }
  }
  /**
   * Replace:
   * ```json
   * [ { "info": [], ":@": { "abc": "def" } }]
   * ```
   * with:
   * ```json
   * [{"$": { "abc": "def" }, "#name": "info" }]
   * ```
   * see https://github.com/NaturalIntelligence/fast-xml-parser/blob/master/docs/v4/2.XMLparseOptions.md#preserveorder
   * @param data input data
   */
  private static fixupPreserveOrder(data: any): any {

    // we need to extract the root name specially
    if (!Array.isArray(data)) {
      throw Error(`Internal Error: XML parser preserveOrder did not yield an array.`);
    }
    if (data.length !== 1) {
      // we ignore comments, so should only have one element
      throw Error(`Internal Error: XML parser preserveOrder did not yield an array of size 1.`);
    }
    // the root element is special, we copy it into a property
    const rootElement = KeymanXMLReader.fixupPreserveOrderObject(data[0]);
    const rootElementName = rootElement['#name'];
    const out: any = {};
    out[rootElementName] = rootElement;
    return out;
  }

  /** takes an 'object' with a property `:@` containing attrs, and one other property with the object name */
  private static fixupPreserveOrderObject(data: any): any {
    const attrs = data[':@'];
    const mainEntry : any = Object.entries(data).filter(([k,v]) => k !== ':@');
    const [elementName, subItems] = mainEntry[0];
    const out : any = {};
    if ( attrs ) {
      out['$'] = attrs;
    }
    if (!elementName) {
      throw Error(`could not find elementName in ${JSON.stringify(mainEntry[0])}`);
    }
    out['#name'] = elementName;
    if (subItems && subItems.length) {
      out['$$'] = subItems.map((subObject: any) => KeymanXMLReader.fixupPreserveOrderObject(subObject));
      // xml2js duplicated data here, including elements in their 'non-preserved-order' form.
      // we don't read this data, but we're maintaining compatibility here with the read format.
      // example:  emit: […], keystroke:[…]
      for (const o of out['$$']) {
        const subElementName = o['#name'];
        const nonPreservedElements = out[subElementName] = out[subElementName] ?? [];
        const oWithoutName = {...o};
        delete oWithoutName['#name']; // #name is only there in the preserved-order form.
        nonPreservedElements.push(oWithoutName);
      }
    }
    return out;
  }

  public parse(data: string): any {

    const parser = this.parser();
    let result = parser.parse(data, true);
    if (PARSER_OPTIONS[this.type].attributeNamePrefix === '$') {
      result = KeymanXMLReader.fixupDollarAttributes(result);
    } else if (PARSER_OPTIONS[this.type].attributeNamePrefix === '@_') {
      result = KeymanXMLReader.fixupEmptyStringToEmptyObject_keylayout(result);
    } else if (PARSER_OPTIONS[this.type].attributeNamePrefix === '@__') {
      result = KeymanXMLReader.fixupEmptyStringToEmptyObject(result);
    } else if (PARSER_OPTIONS[this.type].preserveOrder) {
      result = KeymanXMLReader.fixupPreserveOrder(result);
    }
    delete result['?xml'];
    return result;
  }

  public parser() {
    const options = PARSER_OPTIONS[this.type];
    if (!options) {
      /* c8 ignore next 1 */
      throw Error(`Internal error: unhandled XML type ${this.type}`);
    }
    return new XMLParser(options);
  }
}

/**
 * Fixed prologue for writing XML
 */
const PROLOGUE = { '?xml': { '$version': '1.0', '$encoding': 'utf-8' } };

/** wrapper for XML generation support */
export class KeymanXMLWriter {

  private static fixDataForWrite(data: any) : any {
    if(typeof data === 'object') {
      if (Array.isArray(data)) {
        // just fixup each item of the array
        return data.map(d => KeymanXMLWriter.fixDataForWrite(d));
      }
      // else object
      const e : any = [];
      Object.entries(data).forEach(([k,v]) => {
        if (k === '$') {
          /* convert $: { a: 1, b: 2 } to { $a: 1, $b: 2} */
          Object.entries(v).forEach(([k,v]) => {
            e.push([`\$${k}`, KeymanXMLWriter.fixDataForWrite(v)]);
          });
        } else {
          e.push([k, KeymanXMLWriter.fixDataForWrite(v)]);
        }
      });
      // reconstitute with $ elements fixed
      return Object.fromEntries(e);
    } else {
      return data; // string or something else
    }
  }

  write(data: any): string {
    const builder = this.builder();
    data = KeymanXMLWriter.fixDataForWrite(data);
    return builder.build({ ...PROLOGUE, ...data });
  }

  constructor(public type: KeymanXMLType) {
  }

  public builder() {
    const options = GENERATOR_OPTIONS[this.type];
    if (!options) {
      /* c8 ignore next 1 */
      throw Error(`Internal error: unhandled XML type ${this.type}`);
    }
    return new XMLBuilder(Object.assign({}, options)); // Shallow clone in case the options are mutated.
  }
}

/**
 * traverse an AJV instancePath and map to an object if possible
 * @param source object tree root (contains the root object)
 * @param path ajv split instancePath, such as '/keyboard3/layers/0'.split('/')
 * @returns undefined if the path was not present, null if path went to something that wasn't an object, otherwise the compileContext object is returned.
 */
export function findInstanceObject(source: any, path: string[]) : any {
  if(!path || !source || path.length == 0) {
    return source;
  } else if(path[0] == '') {
    return findInstanceObject(source, path.slice(1));
  } else if(Array.isArray(source) || typeof source == 'object') {
    const child = source[path[0]];
    if (child == undefined) return child; // nothing here
    if (!child || typeof child == 'string') {
      return source; // return the *parent* object if the child is empty (could be a property)
    }
    return findInstanceObject(child, path.slice(1));
  } else {
    return null;
  }
}

/**
 * Return an object simulating an XML object with an offset number
 * For use in calling message functions
 * @param c number for the offset setting
 * @param x if set, this object will be used as the base object instead of {}
 */
export function withOffset(c: number, compileContext?: any) : KeymanXMLMetadata {
  // set metadata on an empty object
  const o = Object.assign({}, compileContext);
  KeymanXMLReader.setMetaData(o, {
    startIndex: c
  });
  return o;
}
