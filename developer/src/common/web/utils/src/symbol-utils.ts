/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by srl on 2025-03-27
 *
 * Utilities for manipulating Symbol properties
 */

export class SymbolUtils {
    /**
       * Copy symbols from 'from' onto 'onto'
       * @param onto object to copy onto
       * @param from source for symbols
       * @returns the onto object
       */
    public static copySymbols<T>(onto: T, from: any): T {
        const o = onto as any;
        for (const sym of Object.getOwnPropertySymbols(from)) {
            o[sym] = from[sym];
        }
        return onto;
    }

    /** use Object.entries to remove all symbols */
    public static removeSymbols<T>(from: T): T {
        if (Array.isArray(from)) {
            return from.map(o => SymbolUtils.removeSymbols(o)) as T;
        }
        if (typeof from !== "object") return from;
        return Object.fromEntries(Object.entries(from).map(([k, v]) => ([k, SymbolUtils.removeSymbols(v)]))) as T;
    }

}
