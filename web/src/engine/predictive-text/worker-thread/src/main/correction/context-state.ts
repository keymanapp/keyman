import { ContextTokenization } from './context-tokenization.js';

import { LexicalModelTypes } from '@keymanapp/common-types';
import Context = LexicalModelTypes.Context;
import Distribution = LexicalModelTypes.Distribution;
import LexicalModel = LexicalModelTypes.LexicalModel;
import Suggestion = LexicalModelTypes.Suggestion;
import Transform = LexicalModelTypes.Transform;

/**
 * Represents a state of the active context at some point in time along with the
 * results of all related, reusable predictive-text operations.
 */
export class ContextState {
  /**
   * The context window in view for the represented Context state,
   * as passed between the predictive-text worker and its host.
   */
  context: Context;

  /**
   * The active lexical model operating upon the Context.
   */
  model: LexicalModel;

  /**
   * Denotes the most likely tokenization for the represented Context.
   */
  tokenization: ContextTokenization;

  /**
   * Denotes the keystroke-sourced Transform that was last applied to a
   * prior ContextState.
   *
   * Note:  if this specific ContextState resulted from applying a
   * Suggestion, this may not match text seen in the current Context!
   */
  appliedInput?: Transform;

  /**
   * Denotes all keystroke data contributing to ContextTokens seen in
   * .tokenization.  For each contributing context transition, its ID
   * may be used to retrieve the original fat-finger distribution for
   * potential keystroke effects.
   */
  inputTransforms: Map<number, Distribution<Transform>>;

  /**
   * The full set of Suggestions produced for the transition to this context state.
   */
  suggestions: Suggestion[];

  /**
   * If set, denotes the suggestion ID for the suggestion (from .suggestions) that
   * was applied for the final transition to this context state.
   */
  appliedSuggestionId?: number;

  /**
   * Indicates whether or not the applied suggestion (if it exists) was applied
   * directly by the user.
   *
   * - `true` if directly applied via banner interaction or other explicitly-intended
   * behaviors
   * - `false` if indirectly applied (say, by triggering whitespace/punctuation input)
   * - `undefined` if no suggestion has been applied.
   */
  isManuallyApplied?: boolean;

  constructor(stateToClone: ContextState);
  constructor(context: Context, model: LexicalModel);
  constructor(param1: Context | ContextState, model?: LexicalModel) {
    if(!(param1 instanceof ContextState)) {
      const context = param1;
      this.context = context;
      this.model = model;
    } else {
      const stateToClone = param1;

      Object.assign(this, stateToClone);
      this.inputTransforms = new Map(stateToClone.inputTransforms);
      this.tokenization = new ContextTokenization(stateToClone.tokenization);

      // A shallow copy of the array is fine, but we'd be best off
      // not aliasing the array itself.
      this.suggestions = [].concat(stateToClone.suggestions);
    }
  }
}