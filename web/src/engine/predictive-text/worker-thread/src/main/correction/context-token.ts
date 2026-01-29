/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-07-30
 *
 * Represents cached data about one token (either a word or a unit of whitespace)
 * in the context and associated correction-search progress and results.
 */

import { LexicalModelTypes } from '@keymanapp/common-types';

import { SearchQuotientNode, PathInputProperties } from "./search-quotient-node.js";
import { TokenSplitMap } from "./context-tokenization.js";
import { LegacyQuotientSpur } from "./legacy-quotient-spur.js";
import { LegacyQuotientRoot } from "./legacy-quotient-root.js";
import { generateSubsetId } from './tokenization-subsets.js';

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
  public get searchModule(): SearchQuotientNode {
    return this._searchModule;
  }
  private _searchModule: SearchQuotientNode;

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
      this._searchModule = priorToken.searchModule;
    } else {
      const model = param;

      // May be altered outside of the constructor.
      this.isWhitespace = false;
      this.isPartial = !!isPartial;

      rawText ||= '';

      // Supports the old pathway for: updateWithBackspace(tokenText: string, transformId: number)
      // Build a token that represents the current text with no ambiguity - probability at max (1.0)
      let searchModule: SearchQuotientNode = new LegacyQuotientRoot(model);
      const BASE_PROBABILITY = 1;
      textToCharTransforms(rawText).forEach((transform) => {
        let inputMetadata: PathInputProperties = {
          segment: {
            start: 0,
            transitionId: undefined
          },
          bestProbFromSet: BASE_PROBABILITY,
          subsetId: generateSubsetId()
        };
        searchModule = new LegacyQuotientSpur(searchModule, [{sample: transform, p: BASE_PROBABILITY}], inputMetadata);
      });

      this._searchModule = searchModule;
    }
  }

  /**
   * Call this to record the original keystroke Transforms for the context range
   * corresponding to this token.
   */
  addInput(inputSource: PathInputProperties, distribution: Distribution<Transform>) {
    this._searchModule = new LegacyQuotientSpur(this._searchModule, distribution, inputSource);
  }

  get inputCount() {
    return this._searchModule.inputCount;
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
  get inputSegments() {
    return this.searchModule.inputSegments;
  }

  /**
   * Gets the unique identifier that may be used to match this ContextToken with
   * a correction-search result.
   */
  get spaceId(): number {
    return this.searchModule.spaceId;
  }

  /**
   * Gets a compact string-based representation of `inputRange` that
   * maps compatible token source ranges to each other.
   */
  get sourceRangeKey(): string {
    return this.searchModule.sourceRangeKey;
  }

  /**
   * Generates text corresponding to the net effects of the most likely inputs
   * received that can correspond to the current instance.
   */
  get exampleInput(): string {
    return this.searchModule.bestExample.text;
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
      resultToken._searchModule = resultToken._searchModule.merge(next._searchModule);
    }

    return resultToken;
  }

  /**
   * Splits this token into multiple tokens as defined by a `TokenSplitMap`.
   * @param split
   * @param lexicalModel
   * @returns
   */
  split(split: TokenSplitMap): ContextToken[] {
    // Split from tail to head - leave as much 'head' intact as possible at each
    // step, rather than needing to reconstruct the tail multiple times.
    const splitSpecs = split.matches.slice();
    let searchSpace = this.searchModule;
    let searchSplits: SearchQuotientNode[] = [];
    while(splitSpecs.length > 0) {
      const spec = splitSpecs.pop();
      if(splitSpecs.length == 0 && spec.textOffset == 0) {
        searchSplits.push(searchSpace);
      } else {
        // Note:  it is conceivable for a token to split into multiple potential
        // tokens due to variations in how the text's construction proceeded up
        // to the point of the split.
        //
        // For now, as a stopgap, we simply take the first such split and roll with that.
        const splitSpaces = searchSpace.split(spec.textOffset)[0];
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
      token._searchModule = path;
      return token;
    });

    return tokensFromSplit;
  }
}