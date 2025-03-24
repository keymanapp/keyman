/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by srl on 2024-09-27
 *
 * Abstraction for XML reading and writing
 */

import { XMLParser, XMLBuilder } from 'fast-xml-parser';

export type KeymanXMLType =
  'keyboard3'           // LDML <keyboard3>
  | 'keyboardTest3'       // LDML <keyboardTest3>
  | 'kps'                 // <Package>
  | 'kvks'                // <visualkeyboard>
  | 'kpj'                 // <KeymanDeveloperProject>
  ;

/** Bag of options, maximally one for each KeymanXMLType */
type KeymanXMLOptionsBag = {
  [key in KeymanXMLType]?: any
};

/** map of options for the XML parser */
const PARSER_OPTIONS: KeymanXMLOptionsBag = {
  'keyboard3': {
    ignoreAttributes: false, // We'd like attributes, please
    attributeNamePrefix: '@__', // We'll use this to convert attributes to strings and subobjects to arrays, when empty.
    trimValues: false, // preserve spaces, but:
    htmlEntities: true,
    tagValueProcessor: (tagName: string, tagValue: string /*, jPath, hasAttributes, isLeafNode*/) => {
      // since trimValues: false, we need to zap any element values that would be trimmed.
      // currently, the LDML spec doesn't have any element values, but this
      // future-proofs us a little in that element values are allowed, just trimmed.
      // if we do need elements in the future, we'd check the preserve-space attribute here.
      return tagValue?.trim();
    },
  },
  'keyboardTest3': {
    ignorePiTags: true,
    htmlEntities: true,
    ignoreAttributes: false, // We'd like attributes, please
    attributeNamePrefix: '', // avoid @_
    preserveOrder: true,     // Gives us a 'special' format
  },
  'kps': {
    ignorePiTags: true,
    ignoreAttributes: false,
    htmlEntities: true,
    attributeNamePrefix: '$', // causes remapping into $: { … } objects
    textNodeName: '_',
    numberParseOptions: {
      skipLike: /(?:)/, // parse numbers as strings
      hex: null,
      leadingZeros: null,
      eNotation: null,
    },
  },
  'kpj': {
    ignorePiTags: true,
    textNodeName: '_',
    htmlEntities: true,
    ignoreAttributes: false, // We'd like attributes, please
    attributeNamePrefix: '', // to avoid '@_' prefixes
    numberParseOptions: {
      skipLike: /(?:)/, // parse numbers as strings
      hex: null,
      leadingZeros: null,
      eNotation: null,
    },
  },
  'kvks': {
    ignorePiTags: true,
    textNodeName: '_',
    htmlEntities: true,
    ignoreAttributes: false, // We'd like attributes, please
    attributeNamePrefix: '$', // causes remapping into $: { … } objects
    numberParseOptions: {
      skipLike: /(?:)/, // parse numbers as strings
      hex: null,
      leadingZeros: null,
      eNotation: null,
    },
    trimValues: false, // preserve spaces, but:
    tagValueProcessor: (tagName: string, tagValue: string, jPath: string, hasAttributes: string, isLeafNode: boolean) : string | undefined => {
      if (!isLeafNode) {
        return tagValue?.trim(); // trimmed value
      } else {
        return null;  // no change to leaf nodes
      }
    },
  },
};

const GENERATOR_OPTIONS: KeymanXMLOptionsBag = {
  kvks: {
    attributeNamePrefix: '$',
    ignoreAttributes: false,
    format: true,
    textNodeName: '_',
    suppressEmptyNode: true,
  },
  kpj: {
    attributeNamePrefix: '$',
    ignoreAttributes: false,
    format: true,
    textNodeName: '_',
    suppressEmptyNode: true,
  },
  kps: {
    attributeNamePrefix: '$',
    ignoreAttributes: false,
    format: true,
    textNodeName: '_',
    suppressEmptyNode: true,
  },
};

/** wrapper for XML parsing support */
export class KeymanXMLReader {
  public constructor(public type: KeymanXMLType) {
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
        e.push(['$', Object.fromEntries(attrs)]);
      }
      return Object.fromEntries(e);
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
    } else if (PARSER_OPTIONS[this.type].attributeNamePrefix === '@__') {
      result = KeymanXMLReader.fixupEmptyStringToEmptyObject(result);
    } else if (PARSER_OPTIONS[this.type].preserveOrder) {
      result = KeymanXMLReader.fixupPreserveOrder(result);
    }
    delete result['?xml'];
    return result;
  }

  public parser() {
    let options = PARSER_OPTIONS[this.type];
    if (!options) {
      /* c8 ignore next 1 */
      throw Error(`Internal error: unhandled XML type ${this.type}`);
    }
    options = Object.assign({}, options); // TODO: xml2js likes to mutate the options here. Shallow clone the object.
    if (options.emptyTag) {
        options.emptyTag = {}; // TODO: xml2js likes to mutate the options here. Reset it.
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

