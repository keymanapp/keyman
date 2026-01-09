/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-07-30
 *
 * Represents cached data about one token (either a word or a unit of whitespace)
 * in the context and associated correction-search progress and results.
 */

import { applyTransform, buildMergedTransform } from "@keymanapp/models-templates";
import { LexicalModelTypes } from '@keymanapp/common-types';
import { deepCopy, KMWString } from "@keymanapp/web-utils";

import { SearchQuotientSpur } from "./search-quotient-spur.js";
import { TokenSplitMap } from "./context-tokenization.js";

import Distribution = LexicalModelTypes.Distribution;
import LexicalModel = LexicalModelTypes.LexicalModel;
import Transform = LexicalModelTypes.Transform;

/**
 * Notes critical properties of the inputs comprising each ContextToken.
 */
export interface TokenInputSource {
  trueTransform: Transform;
  inputStartIndex: number;
  bestProbFromSet: number;
}

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
  public get searchModule(): SearchQuotientSpur {
    return this._searchModule;
  }
  private _searchModule: SearchQuotientSpur;

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
   * Represents the original, 'true' input transforms (tokenized, as necessary)
   * applied to the actual context for the set of keystrokes contributing to
   * this token.
   */
  private _inputRange: TokenInputSource[];

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
      this.isWhitespace = priorToken.isWhitespace;
      this.isPartial = priorToken.isPartial;

      // We need to construct a separate search space from other token copies.
      //
      // In case we are unable to perfectly track context (say, due to multitaps)
      // we need to ensure that only fully-utilized keystrokes are considered.
      this._searchModule = priorToken.searchModule;
      this._inputRange = priorToken._inputRange.slice();

      // Preserve any annotated applied-suggestion transition ID data; it's useful
      // for delayed reversion operations.
      if(priorToken.appliedTransitionId !== undefined) {
        this.appliedTransitionId = priorToken.appliedTransitionId
      }
    } else {
      const model = param;

      // May be altered outside of the constructor.
      this.isWhitespace = false;
      this.isPartial = !!isPartial;
      this._inputRange = [];

      rawText ||= '';

      // Supports the old pathway for: updateWithBackspace(tokenText: string, transformId: number)
      // Build a token that represents the current text with no ambiguity - probability at max (1.0)
      let searchSpace = new SearchQuotientSpur(model);
      const BASE_PROBABILITY = 1;
      textToCharTransforms(rawText).forEach((transform) => {
        this._inputRange.push({
          trueTransform: transform,
          inputStartIndex: 0,
          bestProbFromSet: BASE_PROBABILITY
        });
        searchSpace = searchSpace.addInput([{sample: transform, p: BASE_PROBABILITY}], 1);
      });

      this._searchModule = searchSpace;
    }
  }

  /**
   * Call this to record the original keystroke Transforms for the context range
   * corresponding to this token.
   */
  addInput(inputSource: TokenInputSource, distribution: Distribution<Transform>) {
    this._inputRange.push(inputSource);
    this._searchModule = this._searchModule.addInput(distribution, inputSource.bestProbFromSet);
  }

  /**
   * Denotes the original keystroke Transforms comprising the range corresponding
   * to this token.
   */
  get inputRange(): Readonly<TokenInputSource[]> {
    return this._inputRange;
  }

  /**
   * Indicates whether or not this ContextToken likely represents an empty token.
   */
  get isEmptyToken(): boolean {
    return this.exampleInput == '';
  }

  /**
   * Gets a compact string-based representation of `inputRange` that
   * maps compatible token source ranges to each other.
   */
  get sourceRangeKey(): string {
    const components: string[] = [];

    for(const source of this.inputRange) {
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
    const composite = this._inputRange.reduce((accum, current) => {
      const alteredTransform = {...current.trueTransform};
      alteredTransform.insert = alteredTransform.insert.slice(current.inputStartIndex);
      return buildMergedTransform(accum, current.trueTransform)
    }, { insert: '', deleteLeft: 0 });
    const prefix = '\u{2421}'.repeat(composite.deleteLeft);
    return prefix + composite.insert;
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
  static merge(tokensToMerge: ContextToken[], lexicalModel: LexicalModel): ContextToken {
    // Assumption:  if we're merging a token, it's not whitespace.
    // Thus, we don't set the .isWhitespace flag field.
    const resultToken = new ContextToken(lexicalModel);

    let lastSourceInput: TokenInputSource;
    let lastInputDistrib: Distribution<Transform>;
    for(const token of tokensToMerge) {
      const inputCount = token.inputRange.length;
      let startIndex = 0;

      if(token.inputRange.length == 0) {
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
            sample: buildMergedTransform(entry.sample, token.searchModule.inputSequence[0][index].sample),
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
        resultToken.addInput(token.inputRange[i], token.searchModule.inputSequence[i]);
      }
      lastSourceInput = token.inputRange[inputCount-1];
      lastInputDistrib = token.searchModule.inputSequence[inputCount-1];
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
  split(split: TokenSplitMap, lexicalModel: LexicalModel) {
    // Assumption:  if we're splitting a token, it's not whitespace - and
    // neither are the spun-off tokens.  Thus, we don't set the .isWhitespace
    // flag field.
    const tokensFromSplit: ContextToken[] = [];

    // Build an alternate version of the transforms:  if we preprocess all deleteLefts,
    // what text remains from each?
    const alteredSources = preprocessInputSources(this.inputRange);

    const blankContext = { left: '', startOfBuffer: true, endOfBuffer: true };
    const splitSpecs = split.matches.slice();
    let currentText = {...blankContext};
    let lenBeforeLastApply = 0;
    let committedLen = 0;
    let constructingToken = new ContextToken(lexicalModel);
    let backupToken: ContextToken;
    let transformIndex = 0;
    while(splitSpecs.length > 0) {
      const splitMatch = splitSpecs[0];

      if(splitMatch.text == currentText.left) {
        tokensFromSplit.push(constructingToken);
        constructingToken = new ContextToken(lexicalModel);
        backupToken = null;
        committedLen += lenBeforeLastApply;
        currentText = {...blankContext};
        splitSpecs.shift();
        continue;
      } else if(currentText.left.indexOf(splitMatch.text) > -1) {
        // Oh dear - we've overshot the target! The split is awkward, in the
        // middle of a keystroke.

        // Restore!
        const overextendedToken = constructingToken;
        constructingToken = backupToken;

        // We know how much of the next transform to pull in:  it's specified on
        // the split object.  Excess on constructed token - the split 'text offset'
        const totalLenBeforeLastApply = committedLen + lenBeforeLastApply;
        // We read the start position for the NEXT token to know the split position.
        const extraCharsAdded = splitSpecs[1].textOffset - totalLenBeforeLastApply;
        const tokenSequence = overextendedToken.searchModule.inputSequence;
        const lastInputIndex = tokenSequence.length - 1;
        const inputDistribution = tokenSequence[lastInputIndex];
        const headDistribution = inputDistribution.map((m) => {
          return {
            sample: {
              ...m.sample,
              insert: KMWString.substring(m.sample.insert, 0, extraCharsAdded),
              deleteRight: 0
            }, p: m.p
          };
        });
        const tailDistribution = inputDistribution.map((m) => {
          return {
            sample: {
              ...m.sample,
              insert: KMWString.substring(m.sample.insert, extraCharsAdded),
              deleteLeft: 0
            }, p: m.p
          };
        });

        const priorSourceInput = overextendedToken.inputRange[lastInputIndex];
        constructingToken.addInput(priorSourceInput, headDistribution);
        tokensFromSplit.push(constructingToken);

        constructingToken = new ContextToken(lexicalModel);
        backupToken = new ContextToken(constructingToken);
        constructingToken.addInput({
          trueTransform: priorSourceInput.trueTransform,
          inputStartIndex: priorSourceInput.inputStartIndex + extraCharsAdded,
          bestProbFromSet: priorSourceInput.bestProbFromSet
        }, tailDistribution);

        const lenToCommit = lenBeforeLastApply + extraCharsAdded;
        splitSpecs.shift();

        committedLen += lenToCommit;
        currentText.left = KMWString.substring(currentText.left, lenToCommit);
        lenBeforeLastApply = 0;
        continue; // without incrementing transformIndex - we haven't processed a new one!
      } else if(transformIndex == alteredSources.length) {
        throw new Error("Invalid split specified!");
      }

      backupToken = new ContextToken(constructingToken);
      lenBeforeLastApply = KMWString.length(currentText.left);
      currentText = applyTransform(alteredSources[transformIndex].trueTransform, currentText);
      constructingToken.addInput(this.inputRange[transformIndex], this.searchModule.inputSequence[transformIndex]);
      transformIndex++;
    }

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