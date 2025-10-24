/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-07-30
 *
 * Represents cached data about one token (either a word or a unit of whitespace)
 * in the context and associated correction-search progress and results.
 */

import { buildMergedTransform } from "@keymanapp/models-templates";
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
  public get searchPath(): SearchSpace {
    return this._searchPath;
  }
  private _searchPath: SearchSpace;

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
      this._searchPath = priorToken.searchPath;
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
        const priorSpace = searchSpace;
        searchSpace = new SearchPath(searchSpace, entry, {
          trueTransform: entry[0].sample,
          inputStartIndex: 0,
          bestProbFromSet: 1
        });
        priorSpace.stopTrackingResults();
      });

      this._searchPath = searchSpace;
    }
  }

  /**
   * Call this to record the original keystroke Transforms for the context range
   * corresponding to this token.
   */
  addInput(inputSource: TokenInputSource, distribution: Distribution<Transform>) {
    const priorSpace = this._searchPath;
    this._searchPath = new SearchPath(this._searchPath, distribution, inputSource);
    priorSpace.stopTrackingResults();
  }

  get inputCount() {
    return this._searchPath.inputCount;
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
    return this.searchPath.sourceIdentifiers;
  }

  /**
   * Gets the unique identifier that may be used to match this ContextToken with
   * a correction-search result.
   */
  get spaceId(): number {
    return this.searchPath.spaceId;
  }

  /**
   * Gets a compact string-based representation of `inputRange` that
   * maps compatible token source ranges to each other.
   */
  get sourceRangeKey(): string {
    const components: string[] = [];
    const sources = this.searchPath.sourceIdentifiers;

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
    return this.searchPath.likeliestSourceText;
  }

  /**
   * Generates text corresponding to the net effects of the most likely inputs
   * received that can correspond to the current instance.
   */
  get exampleInput(): string {
    return this.searchPath.bestExample.text;
  }

  /**
   * Merges multiple tokens into a single composite token, checking for any previously-split
   * Transforms at the boundaries and remerging them as appropriate.
   * @param split
   * @param lexicalModel
   * @returns
   */
  static merge(tokensToMerge: ContextToken[], lexicalModel: LexicalModel): ContextToken {
    // Assumption:  if we're merging a token, it's not whitespace.
    // Thus, we don't set the .isWhitespace flag field.
    const resultToken = new ContextToken(lexicalModel);

    let lastSourceInput: TokenInputSource;
    let lastInputDistrib: Distribution<Transform>;
    for(const token of tokensToMerge) {
      const inputCount = token.inputCount;
      let startIndex = 0;

      if(inputCount == 0) {
        continue;
      }

      // Are we re-merging on a previously split transform?
      if(lastSourceInput?.trueTransform != token.inputRange[0].trueTransform) {
        if(lastSourceInput) {
          resultToken.addInput(lastSourceInput, lastInputDistrib);
        } // else:  there's nothing to add as input
      } else {
        // If so, re-merge it!
        startIndex++;

        lastInputDistrib = lastInputDistrib?.map((entry, index) => {
          return {
            sample: buildMergedTransform(entry.sample, token.searchPath.inputSequences[0][0][index].sample),
            p: entry.p
          }
        });

        // In case there's only one input that needs merging on both ends.
        if(inputCount == 1) {
          // There's potential that the next incoming token needs to merge with this.
          continue;
        } else {
          resultToken.addInput(lastSourceInput, lastInputDistrib);
        }
      }
      lastSourceInput = null;
      lastInputDistrib = null;

      // Ignore the last entry for now - it may need to merge with a matching
      // entry in the next token!
      for(let i = startIndex; i < inputCount - 1; i++) {
        resultToken.addInput(token.inputRange[i], token.searchPath.inputSequences[0][i]);
      }
      lastSourceInput = token.inputRange[inputCount-1];
      lastInputDistrib = token.searchPath.inputSequences[0][inputCount-1];
    }

    resultToken.addInput(lastSourceInput, lastInputDistrib);

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
    let searchSpace = this.searchPath;
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
      token._searchPath = path;
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