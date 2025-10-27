/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-07-30
 *
 * Represents cached data about one token (either a word or a unit of whitespace)
 * in the context and associated correction-search progress and results.
 */

import { LexicalModelTypes } from '@keymanapp/common-types';
import { deepCopy, KMWString } from "@keymanapp/web-utils";

import { SearchPath } from "./search-path.js";
import { SearchSpace, TokenInputSource } from "./search-space.js";
import { TokenSplitMap } from "./context-tokenization.js";

import Distribution = LexicalModelTypes.Distribution;
import LexicalModel = LexicalModelTypes.LexicalModel;
import Transform = LexicalModelTypes.Transform;

/**
 * Breaks apart a raw text string into individual, single-codepoint
 * transforms, all set with the specified transform ID.
 *
 * This is designed for use when initializing a new ContextToken without
 * any prior cached data or for rewriting its probabilities after
 * receiving backspace input.
 * @param text
 * @param transformId
 * @returns
 */
function textToCharTransforms(text: string, transformId?: number): Transform[] {
  return transformId ?
    [...text].map(insert => ({insert, deleteLeft: 0, id: transformId})) :
    [...text].map(insert => ({insert, deleteLeft: 0}));
}

/**
 * Represents cached data about one token (either a word or a unit of whitespace)
 * in the context and associated correction-search progress and results.
 */
export class ContextToken {
  /**
   * Indicates whether or not the token is considered whitespace.
   */
  isWhitespace: boolean;

  /**
   * Contains all relevant correction-search data for use in generating
   * corrections for this ContextToken instance.
   */
  public get searchSpace(): SearchSpace {
    return this._searchSpace;
  }
  private _searchSpace: SearchSpace;

  isPartial: boolean;

  /**
   * Tokens affected by applied suggestions will indicate the transition ID of
   * the applied suggestion here.
   *
   * Is `undefined` if no suggestion was applied to the context portion
   * represented by this token.
   */
  appliedTransitionId?: number;

  /**
   * Constructs a new, empty instance for use with the specified LexicalModel.
   * @param model
   */
  constructor(model: LexicalModel);
  /**
   * Constructs a new instance with pre-existing text for use with the specified LexicalModel.
   * @param model
   * @param rawText
   */
  constructor(model: LexicalModel, rawText: string, isPartial?: boolean);
  /**
   * This constructor deep-copies the specified instance.
   * @param baseToken
   */
  constructor(baseToken: ContextToken);
  constructor(param: ContextToken | LexicalModel, rawText?: string, isPartial?: boolean) {
    if(param instanceof ContextToken) {
      const priorToken = param;
      Object.assign(this, priorToken);

      // We need to construct a separate search space from other token copies.
      //
      // In case we are unable to perfectly track context (say, due to multitaps)
      // we need to ensure that only fully-utilized keystrokes are considered.
      this._searchSpace = priorToken.searchSpace;
    } else {
      const model = param;

      // May be altered outside of the constructor.
      this.isWhitespace = false;
      this.isPartial = !!isPartial;

      rawText ||= '';

      // Supports the old pathway for: updateWithBackspace(tokenText: string, transformId: number)
      const rawTransformDistributions: Distribution<Transform>[] = textToCharTransforms(rawText).map(function(transform) {
        return [{sample: transform, p: 1.0}];
      });

      let searchSpace = new SearchPath(model);

      rawTransformDistributions.forEach((entry) => {
        searchSpace = new SearchPath(searchSpace, entry, {
          trueTransform: entry[0].sample,
          inputStartIndex: 0,
          bestProbFromSet: 1
        });
      });

      this._searchSpace = searchSpace;
    }
  }

  /**
   * Call this to record the original keystroke Transforms for the context range
   * corresponding to this token.
   */
  addInput(inputSource: TokenInputSource, distribution: Distribution<Transform>) {
    this._searchSpace = new SearchPath(this._searchSpace, distribution, inputSource);
  }

  get inputCount() {
    return this._searchSpace.inputCount;
  }

  /**
   * Indicates whether or not this ContextToken likely represents an empty token.
   */
  get isEmptyToken(): boolean {
    return this.exampleInput == '';
  }

  /**
   * Denotes the original keystroke Transforms comprising the range corresponding
   * to this token.
   */
  get inputRange() {
    return this.searchSpace.sourceIdentifiers;
  }

  /**
   * Gets the unique identifier that may be used to match this ContextToken with
   * a correction-search result.
   */
  get spaceId(): number {
    return this.searchSpace.spaceId;
  }

  /**
   * Gets a compact string-based representation of `inputRange` that
   * maps compatible token source ranges to each other.
   */
  get sourceRangeKey(): string {
    const components: string[] = [];
    const sources = this.searchSpace.sourceIdentifiers;

    for(const source of sources) {
      const i = source.inputStartIndex;
      components.push(`T${source.trueTransform.id}${i != 0 ? '@' + i : ''}`);
    }

    return components.join('+');
  }

  /**
   * Gets a simple, compact string-based representation of `inputRange`.
   *
   * This should only ever be used for debugging purposes.
   */
  get sourceText(): string {
    return this.searchSpace.likeliestSourceText;
  }

  /**
   * Generates text corresponding to the net effects of the most likely inputs
   * received that can correspond to the current instance.
   */
  get exampleInput(): string {
    return this.searchSpace.bestExample.text;
  }

  /**
   * Merges multiple tokens into a single composite token, checking for any previously-split
   * Transforms at the boundaries and remerging them as appropriate.
   * @param split
   * @param lexicalModel
   * @returns
   */
  static merge(tokensToMerge: ContextToken[]): ContextToken {
    if(tokensToMerge.length < 1) {
      return null;
    }

    // Assumption:  if we're merging a token, it's not whitespace.
    // Thus, we don't set the .isWhitespace flag field.
    const resultToken = new ContextToken(tokensToMerge.shift());
    while(tokensToMerge.length > 0) {
      const next = tokensToMerge.shift();
      resultToken._searchSpace = resultToken._searchSpace.merge(next._searchSpace);
    }

    return resultToken;
  }

  /**
   * Splits this token into multiple tokens as defined by a `TokenSplitMap`.
   * @param split
   * @param lexicalModel
   * @returns
   */
  split(split: TokenSplitMap) {
    // Split from tail to head - leave as much 'head' intact as possible at each
    // step, rather than needing to reconstruct the tail multiple times.
    const splitSpecs = split.matches.slice();
    let searchSpace = this.searchSpace;
    let searchSplits: SearchSpace[] = [];
    while(splitSpecs.length > 0) {
      const spec = splitSpecs.pop();
      if(splitSpecs.length == 0 && spec.textOffset == 0) {
        searchSplits.push(searchSpace);
      } else {
        const splitSpaces = searchSpace.split(spec.textOffset);
        searchSplits.push(splitSpaces[1]);
        searchSpace = splitSpaces[0];
      }
    }
    searchSplits.reverse();

    // Build new tokens based on the split paths.
    //
    // Assumption:  if we're splitting a token, it's not whitespace - and
    // neither are the spun-off tokens.  Thus, we don't set the .isWhitespace
    // flag field.

    const tokensFromSplit = searchSplits.map((path) => {
      const token = new ContextToken(this);
      token._searchSpace = path;
      return token;
    });

    return tokensFromSplit;
  }
}

export function preprocessInputSources(inputSources: ReadonlyArray<TokenInputSource>) {
  const alteredSources = deepCopy(inputSources);
  let trickledDeleteLeft = 0;
  for(let i = alteredSources.length - 1; i >= 0; i--) {
    const source = alteredSources[i];
    if(trickledDeleteLeft) {
      const insLen = KMWString.length(source.trueTransform.insert);
      if(insLen <= trickledDeleteLeft) {
        source.trueTransform.insert = '';
        trickledDeleteLeft -= insLen;
      } else {
        source.trueTransform.insert = KMWString.substring(source.trueTransform.insert, 0, insLen - trickledDeleteLeft);
        trickledDeleteLeft = 0;
      }
    }
    trickledDeleteLeft += source.trueTransform.deleteLeft;
    source.trueTransform.deleteLeft = 0;
  }

  alteredSources[0].trueTransform.deleteLeft = trickledDeleteLeft;
  return alteredSources;
}