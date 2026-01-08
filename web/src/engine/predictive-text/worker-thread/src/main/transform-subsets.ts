/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-08-21
 *
 * This file defines a method used to 'catalog' transforms based on the area of
 * the context that they impact, allowing groups of similar transforms to be
 * handled as related groups more easily.
 */

import { LexicalModelTypes } from '@keymanapp/common-types';
import { KMWString } from '@keymanapp/web-utils';

import Distribution = LexicalModelTypes.Distribution;
import Transform = LexicalModelTypes.Transform;

/**
 * Represents a subset of Transforms, taken from the distribution of input
 * Transform, where all members meet certain criteria that others from the
 * source distribution do not.
 * @typeParam Key - the type of the criteria uniquely keying this Transform
 * subset.
 */
export interface TransformSubset<Key> {
  /**
   * The value of whatever property was used as a criteria when building this
   * distinct subset.
   */
  key: Key;

  /**
   * All Transforms meeting the criteria defining this TransformSubset
   */
  entries: Distribution<Transform>;

  /**
   * The summed probability mass for all Transforms in the subset
   */
  cumulativeMass: number;

  /**
   * The currently-processed and commonly-held portion of the `insert` field
   * for all Transforms in this subset.
   */
  insert: string;
}

/**
 * Places all Transforms in the distribution into distinct subsets where BOTH
 * the following properties match for all entries in the same set:
 * 1. `deleteLeft`
 * 2. `insert.length`
 *
 * The subset instances are keyed by just `insert.length`, despite both serving
 * as criteria for building the subsets.
 *
 * It also computes the sum total probability mass represented by the set.
 *
 * Array index: the deleteLeft value (which is always non-negative) Inner map
 * index:  the length of the insert string.
 * @param distribution
 */
export function subsetByInterval(
  distribution: Distribution<Transform>
): TransformSubset<number>[][] {
  const returnRoot: TransformSubset<number>[][] = [];

  distribution.forEach((entry) => {
    const { sample, p } = entry;

    const dl = sample.deleteLeft;
    const deleteBin = returnRoot[dl] ?? [];

    const ins = KMWString.length(sample.insert);
    const subset: TransformSubset<number> = deleteBin[ins] ?? { key: ins, insert: "", entries: [], cumulativeMass: 0 };

    subset.entries.push(entry);
    subset.cumulativeMass += p;

    deleteBin[ins] = subset;
    returnRoot[dl] = deleteBin;
  });

  return returnRoot;
}

/**
 * Places all Transforms in the distribution into distinct subsets where the
 * character at the specified index within their Transform matches.
 *
 * It also computes the sum total probability mass represented by the set.
 *
 * Map key:  the character shared (at the specified subindex) by each Transform
 * in the subset
 * @param intervalSubset
 * @param subIndex
 */
export function subsetByChar(intervalSubset: TransformSubset<number>, subIndex: number, toKey?: (s: string) => string): Map<string, TransformSubset<string>> {
  toKey = toKey ?? (x => x);
  const returnRoot: Map<string, TransformSubset<string>> = new Map();

  // If there the input catalog doesn't support the subindex, just return everything in
  // one bulk catalog.
  if(intervalSubset.key <= subIndex) {
    const everything: TransformSubset<string> = {...intervalSubset, key: ''};
    returnRoot.set('', everything);
    return returnRoot;
  }

  intervalSubset.entries.forEach((entry) => {
    const { sample, p } = entry;
    const char = toKey(KMWString.charAt(sample.insert, subIndex));

    const subset: TransformSubset<string> = returnRoot.get(char) ?? { key: char, insert: intervalSubset.insert, entries: [], cumulativeMass: 0 };

    subset.entries.push(entry);
    subset.cumulativeMass += p;

    returnRoot.set(char, subset);
  });

  return returnRoot;
}

/**
 * Merges two or more TransformCatalog instances into a single instance keyed by
 * the value passed in as a parameter.
 * @param subsets
 * @param key
 * @returns
 */
export function mergeSubset<T, K>(subsets: TransformSubset<T>[], key: K): TransformSubset<K> {
  return subsets.reduce((accum, current) => {
    accum.entries = accum.entries.concat(current.entries);
    accum.cumulativeMass += current.cumulativeMass;
    return accum;
  }, {
    key: key,
    // May occur during some unit tests (with mocked & limited fat-fingering)
    insert: subsets[0]?.insert,
    entries: [],
    cumulativeMass: 0
  });
}