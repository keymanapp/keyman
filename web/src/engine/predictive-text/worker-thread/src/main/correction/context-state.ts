import { ContextTokenization } from './context-tokenization.js';

import { LexicalModelTypes } from '@keymanapp/common-types';
import Context = LexicalModelTypes.Context;
import Distribution = LexicalModelTypes.Distribution;
import LexicalModel = LexicalModelTypes.LexicalModel;
import Suggestion = LexicalModelTypes.Suggestion;
import Transform = LexicalModelTypes.Transform;
import { ContextToken } from './context-token.js';
import { determineModelTokenizer } from '#./model-helpers.js';

/**
 * Represents a state of the active context at some point in time along with the
 * results of all related, reusable predictive-text operations.
 */
export class ContextState {
  /**
   * The context window in view for the represented Context state,
   * as passed between the predictive-text worker and its host.
   */
  private _context: Context;

  /**
   * The active lexical model operating upon the Context.
   */
  readonly model: LexicalModel;

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

  /**
   * Deep-copies a prior instance.
   * @param stateToClone
   */
  constructor(stateToClone: ContextState);
  /**
   * Initializes a new ContextState instance based on the active model and context.
   *
   * If a precomputed tokenization of the context (with prior correction-search
   * calculation data) is not available, it will be spun up from scratch.
   *
   * @param context The context available within the current sliding context-window
   * @param model The active lexical model.
   * @param tokenization Precomputed tokenization for the context, leveraging previous
   * correction-search progress and results
   */
  constructor(context: Context, model: LexicalModel, tokenization?: ContextTokenization);
  constructor(param1: Context | ContextState, model?: LexicalModel, tokenization?: ContextTokenization) {
    if(!(param1 instanceof ContextState)) {
      const context = param1;
      this._context = context;
      this.model = model;
      if(tokenization) {
        this.tokenization = tokenization;
      } else {
        this.initFromReset();
      }
    } else {
      const stateToClone = param1;

      Object.assign(this, stateToClone);
      this.inputTransforms = new Map(stateToClone.inputTransforms);
      this.tokenization = new ContextTokenization(stateToClone.tokenization);

      // A shallow copy of the array is fine, but we'd be best off
      // not aliasing the array itself.
      if(stateToClone.suggestions?.length ?? 0 > 0) {
        this.suggestions = [].concat(stateToClone.suggestions);
      }
    }
  }

  /**
   * The context window in view for the represented Context state,
   * as passed between the predictive-text worker and its host.
   */
  get context(): Context {
    return this._context;
  }

  /**
   * Initializes the ContextState instance for use when no valid prior
   * information is available - typically, immediately after engine
   * initialization or a context reset.
   */
  private initFromReset() {
    const context = this.context;
    const lexicalModel = this.model;

    const tokenizedContext = determineModelTokenizer(lexicalModel)(context).left;

    let baseTokens = tokenizedContext.map(function(entry) {
      let token = new ContextToken(lexicalModel, entry.text);

      if(entry.isWhitespace) {
        token.isWhitespace = true;
      }

      return token;
    });

    // And now build the final context state object, which includes whitespace 'tokens'.);
    const tokenization: ContextToken[] = [];

    while(baseTokens.length > 0) {
      tokenization.push(baseTokens.splice(0, 1)[0]);
    }

    if(tokenization.length == 0) {
      let token = new ContextToken(lexicalModel);
      tokenization.push(token);
    }

    this.tokenization = new ContextTokenization(tokenization);
  }
}