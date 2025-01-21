/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Placeholder for a dynamic repertoire object.
 */

export type RepertoireEntry = string;

export class FakeRepertoire implements Iterable<RepertoireEntry> {
    constructor(private items: RepertoireEntry[]) {

    }

    // allow `for (x of repertoire)`
    [Symbol.iterator](): Iterator<RepertoireEntry, any, any> {
        // use the string array's iterator
        return this.items[Symbol.iterator]();
    }

    // allow `repertoire.map`
    map(callbackfn: (value: string, index: number, array: string[]) => any, thisArg?: any): any {
        return this.items.map(callbackfn);
    }
}

export const SAMPLE_REPERTOIRE = new FakeRepertoire([
    ..."abcdefghijklmnopqrstuvwxyz".split(''),
]);
