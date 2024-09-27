import { xml2js } from "./index.js";

export class KeymanXMLOptions {
    type: 'keyboard3'         // LDML <keyboard3>
        | 'keyboard3-test'    // LDML <keyboardTest3>
        | 'kps'   // <Package>
        | 'kvks'            // <visualkeyboard>
        | 'kpj' //    // <KeymanDeveloperProject>
        ;
}

/** wrapper for XML parsing support */
export class KeymanXMLReader {
    public constructor(public options: KeymanXMLOptions) {
    }

    public parse(data: string): Object {
        const parser = this.parser();
        let a: any;
        parser.parseString(data, (e: unknown, r: unknown) => { if (e) throw e; a = r; });
        return a;
    }

    public parser() {
        const { type } = this.options;
        switch (type) {
            case 'keyboard3':
                return new xml2js.Parser({
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
            case 'keyboard3-test':
                return new xml2js.Parser({
                    // explicitArray: false,
                    preserveChildrenOrder: true, // needed for test data
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
            case 'kps':
                return new xml2js.Parser({
                    explicitArray: false
                  });
            case 'kpj':
                return new xml2js.Parser({
                    explicitArray: false,
                    mergeAttrs: false,
                    includeWhiteChars: false,
                    normalize: false,
                    emptyTag: ''
                  });
            case 'kvks':
                return new xml2js.Parser({
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
                  });
            default:
                /* c8 ignore next 1 */
                throw Error(`Internal error: unhandled XML type ${type}`);
        }
    }
}

/** wrapper for XML generation support */
export class KeymanXMLWriter {
    constructor(public options: KeymanXMLOptions) {
    }
}

