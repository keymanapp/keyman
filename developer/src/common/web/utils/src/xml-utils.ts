/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by srl on 2024-09-27
 *
 * Abstraction for XML reading and writing
 */

import * as xml2js from "./deps/xml2js/xml2js.js";

export type KeymanXMLType =
  'keyboard3'           // LDML <keyboard3>
  | 'keyboardTest3'       // LDML <keyboardTest3>
  | 'kps'                 // <Package>
  | 'kvks'                // <visualkeyboard>
  | 'kpj'                 // <KeymanDeveloperProject>
  ;

/** Bag of options, maximally one for each KeymanXMLType */
type KemanXMLOptionsBag = {
  [key in KeymanXMLType]?: any
};

/** map of options for the XML parser */
const PARSER_OPTIONS: KemanXMLOptionsBag = {
  'keyboard3': {
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
  },
  'keyboardTest3': {
    preserveChildrenOrder: true, // needed for test data
    explicitChildren: true, // needed for test data
  },
  'kps': {
    explicitArray: false
  },
  'kpj': {
    explicitArray: false,
    mergeAttrs: false,
    includeWhiteChars: false,
    normalize: false,
    emptyTag: ''
  },
  'kvks': {
    explicitArray: false,
    mergeAttrs: false,
    includeWhiteChars: true,
    normalize: false,
    emptyTag: {} as any
    // Why "as any"? xml2js is broken:
    // https://github.com/Leonidas-from-XIV/node-xml2js/issues/648 means
    // that an old version of `emptyTag` is used which doesn't support
    // functions, but DefinitelyTyped is requiring use of function or a
    // string. See also notes at
    // https://github.com/DefinitelyTyped/DefinitelyTyped/pull/59259#issuecomment-1254405470
    // An alternative fix would be to pull xml2js directly from github
    // rather than using the version tagged on npmjs.com.
  },
};

const GENERATOR_OPTIONS: KemanXMLOptionsBag = {
  kvks: {
    allowSurrogateChars: true,
    attrkey: '$',
    charkey: '_',
    xmldec: {
      version: '1.0',
      encoding: 'UTF-8',
      standalone: true
    },
  },
};

/** wrapper for XML parsing support */
export class KeymanXMLReader {
  public constructor(public type: KeymanXMLType) {
  }

  public parse(data: string): any {
    const parser = this.parser();
    let a: any;
    parser.parseString(data, (e: unknown, r: unknown) => { if (e) throw e; a = r; });
    return a;
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
    return new xml2js.Parser(options);
  }
}

/** wrapper for XML generation support */
export class KeymanXMLWriter {
  write(data: any): string {
    const builder = this.builder();
    return builder.buildObject(data);
  }
  constructor(public type: KeymanXMLType) {
  }

  public builder() {
    const options = GENERATOR_OPTIONS[this.type];
    if (!options) {
      /* c8 ignore next 1 */
      throw Error(`Internal error: unhandled XML type ${this.type}`);
    }
    return new xml2js.Builder(Object.assign({}, options)); // Shallow clone in case the options are mutated.
  }
}

