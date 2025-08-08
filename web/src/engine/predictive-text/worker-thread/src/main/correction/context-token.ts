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
  constructor(model: LexicalModel, rawText: string);
  /**
   * This constructor deep-copies the specified instance.
   * @param baseToken
   */
  constructor(baseToken: ContextToken);
  constructor(param: ContextToken | LexicalModel, rawText?: string) {
    if(param instanceof ContextToken) {
      const priorToken = param;
      this.isWhitespace = priorToken.isWhitespace;

      // We need to construct a separate search space from other token copies.
      //
      // In case we are unable to perfectly track context (say, due to multitaps)
      // we need to ensure that only fully-utilized keystrokes are considered.
      this.searchSpace = new SearchSpace(priorToken.searchSpace);
    } else {
      const model = param;

      // May be altered outside of the constructor.
      this.isWhitespace = false;
      this.searchSpace = new SearchSpace(model);

      rawText ||= '';

      // Supports the old pathway for: updateWithBackspace(tokenText: string, transformId: number)
      const rawTransformDistributions: Distribution<Transform>[] = textToCharTransforms(rawText).map(function(transform) {
        return [{sample: transform, p: 1.0}];
      });
      rawTransformDistributions.forEach((entry) => this.searchSpace.addInput(entry));
    }
  }

  /**
   * Displays text corresponding to the net effects of the most likely inputs received
   * that can correspond to the current instance.
   */
  get exampleInput(): string {
    const transforms = this.searchSpace.inputSequence.map((dist) => dist[0].sample)
    const composite = transforms.reduce((accum, current) => buildMergedTransform(accum, current), { insert: '', deleteLeft: 0});
    return composite.insert;
  }
}