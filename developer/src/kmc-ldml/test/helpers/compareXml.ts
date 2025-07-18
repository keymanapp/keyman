import {assert} from 'chai';
import {readFileSync} from 'node:fs';
import { KeymanXMLReader, SymbolUtils } from "@keymanapp/developer-utils";
import { ObjectWithCompileContext } from '@keymanapp/common-types';

/**
 *
 * @param actual path to actual XML
 * @param expect path to expected XML
 * @param mutator optional function that will be applied to the parsed object
 */
export function compareXml(actual : string, expect: string, mutator?: (input: ObjectWithCompileContext) => ObjectWithCompileContext) {
    if (!mutator) {
        // no-op
        mutator = (compileContext: ObjectWithCompileContext) => compileContext;
    }
    const reader = new KeymanXMLReader('keyboard3');

    const actualStr = readFileSync(actual, 'utf-8');
    const expectStr = readFileSync(expect, 'utf-8');

    const actualParsed = mutator(reader.parse(actualStr));
    const expectParsed = mutator(reader.parse(expectStr));

    assert.deepEqual(SymbolUtils.removeSymbols(actualParsed), SymbolUtils.removeSymbols(expectParsed));
}
