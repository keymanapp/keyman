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

import { SearchSpace } from "./distance-modeler.js";

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
  readonly searchSpace: SearchSpace;

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
  private _inputRange: Transform[];

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
      this.searchSpace = new SearchSpace(priorToken.searchSpace);
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
      this.searchSpace = new SearchSpace(model);
      this._inputRange = [];

      rawText ||= '';

      // Supports the old pathway for: updateWithBackspace(tokenText: string, transformId: number)
      const rawTransformDistributions: Distribution<Transform>[] = textToCharTransforms(rawText).map(function(transform) {
        return [{sample: transform, p: 1.0}];
      });
      rawTransformDistributions.forEach((entry) => {
        this._inputRange.push(entry[0].sample);
        this.searchSpace.addInput(entry);
      });
    }
  }

  /**
   * Call this to record the original keystroke Transforms for the context range
   * corresponding to this token.
   */
  addSourceInput(transform: Transform) {
    this._inputRange.push(transform);
  }

  /**
   * Denotes the original keystroke Transforms comprising the range corresponding
   * to this token.
   */
  get inputRange(): Readonly<Transform[]> {
    return this._inputRange;
  }

  /**
   * Gets a simple, human-readable representation of `inputRange`.
   *
   * Should not actually be used in code - its use is intended only for
   * debugging.
   */
  get sourceText(): string {
    const composite = this._inputRange.reduce((accum, current) => buildMergedTransform(accum, current), { insert: '', deleteLeft: 0 });
    const prefix = '\u{2421}'.repeat(composite.deleteLeft);
    return prefix + composite.insert;
  }

  /**
   * Generates text corresponding to the net effects of the most likely inputs
   * received that can correspond to the current instance.
   */
  get exampleInput(): string {
    /*
     * TODO:  with clear limits (strict cost minimization?) / prior calculation
     * attempts, return the best _suggestion_ for this token.  This is
     * especially relevant for epic/dict-breaker - we want to best model the token
     * as it would apply within the word-breaking algorithm.
     *
     * If not possible, find the best of the deepest search paths and append the
     * most likely keystroke data afterward.
     */
    const transforms = this.searchSpace.inputSequence.map((dist) => dist[0].sample)
    const composite = transforms.reduce((accum, current) => buildMergedTransform(accum, current), { insert: '', deleteLeft: 0});
    return composite.insert;
  }
}