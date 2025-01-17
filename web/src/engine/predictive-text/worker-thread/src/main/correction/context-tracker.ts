import { applyTransform, buildMergedTransform, Token } from '@keymanapp/models-templates';

import { ClassicalDistanceCalculation, EditOperation } from './classical-calculation.js';
import { SearchSpace } from './distance-modeler.js';
import TransformUtils from '../transformUtils.js';
import { determineModelTokenizer } from '../model-helpers.js';
import { tokenizeTransform, tokenizeTransformDistribution } from './transform-tokenization.js';
import { LexicalModelTypes } from '@keymanapp/common-types';
import Context = LexicalModelTypes.Context;
import Distribution = LexicalModelTypes.Distribution;
import LexicalModel = LexicalModelTypes.LexicalModel;
import Suggestion = LexicalModelTypes.Suggestion;
import Transform = LexicalModelTypes.Transform;
import USVString = LexicalModelTypes.USVString;

function textToCharTransforms(text: string, transformId?: number) {
  let perCharTransforms: Transform[] = [];

  for(let i=0; i < text.kmwLength(); i++) {
    let char = text.kmwCharAt(i); // is SMP-aware

    let transform: Transform = {
      insert: char,
      deleteLeft: 0,
      id: transformId
    };

    perCharTransforms.push(transform);
  }

  return perCharTransforms;
}

export function getEditPathLastMatch(editPath: EditOperation[]) {
  const editLength = editPath.length;
  // Special handling: appending whitespace to whitespace with the default wordbreaker.
  // The default wordbreaker currently adds an empty token after whitespace; this would
  // show up with 'substitute', 'match' at the end of the edit path.  (This should remain.)
  if(editLength >= 2 && editPath[editLength - 2] == 'substitute' && editPath[editLength - 1] == 'match') {
    return editPath.lastIndexOf('match', editLength - 2);
  } else {
    return editPath.lastIndexOf('match');
  } 
}

export class TrackedContextSuggestion {
  suggestion: Suggestion;
  tokenWidth: number;
}

export class TrackedContextToken {
  raw: string;
  replacementText: string;
  isWhitespace?: boolean;

  transformDistributions: Distribution<Transform>[] = [];
  replacements: TrackedContextSuggestion[] = [];
  activeReplacementId: number = -1;

  constructor();
  constructor(instance: TrackedContextToken);
  constructor(instance?: TrackedContextToken) {
    Object.assign(this, instance);
    // We don't alter the values in replacements, but we do wish to prevent aliasing 
    // of the array containing them.
    if(instance) {
      this.replacements = instance.replacements.slice();
    }
  }

  get currentText(): string {
    if(this.replacementText === undefined || this.replacementText === null) {
      return this.raw;
    } else {
      return this.replacementText;
    }
  }

  get replacement(): TrackedContextSuggestion {
    let replacementId = this.activeReplacementId;
    return this.replacements.find(function(replacement) {
      return replacement.suggestion.id == replacementId;
    });
  }

  clearReplacements() {
    this.activeReplacementId = -1;
    this.replacements = []
  }

  /**
   * Used for 14.0's backspace workaround, which flattens all previous Distribution<Transform>
   * entries because of limitations with direct use of backspace transforms.
   * @param tokenText
   * @param transformId
   */
  updateWithBackspace(tokenText: USVString, transformId: number) {
    // It's a backspace transform; time for special handling!
    //
    // For now, with 14.0, we simply compress all remaining Transforms for the token into
    // multiple single-char transforms.  Probabalistically modeling BKSP is quite complex,
    // so we simplify by assuming everything remaining after a BKSP is 'true' and 'intended' text.
    //
    // Note that we cannot just use a single, monolithic transform at this point b/c
    // of our current edit-distance optimization strategy; diagonalization is currently...
    // not very compatible with that.
    let backspacedTokenContext: Distribution<Transform>[] = textToCharTransforms(tokenText, transformId).map(function(transform) {
      return [{sample: transform, p: 1.0}];
    });

    this.raw = tokenText;
    this.transformDistributions = backspacedTokenContext;  
    this.clearReplacements();
  }

  update(transformDistribution: Distribution<Transform>, tokenText?: USVString) {
    // Preserve existing text if new text isn't specified.
    tokenText = tokenText || (tokenText === '' ? '' : this.raw);

    if(transformDistribution?.length > 0) {
      this.transformDistributions.push(transformDistribution);
    }

    // Replace old token's raw-text with new token's raw-text.
    this.raw = tokenText;
    this.clearReplacements();
  }
}

export class TrackedContextState {
  // Stores the post-transform Context.  Useful as a debugging reference, but also used to
  // pre-validate context state matches in case of discarded changes from multitaps.
  taggedContext: Context;
  model: LexicalModel;

  tokens: TrackedContextToken[];
  /**
   * How many tokens were removed from the start of the best-matching ancestor.
   * Useful for restoring older states, e.g., when the user moves the caret backwards, we can recover the context at that position.
   */
  indexOffset: number;

  // Tracks all search spaces starting at the current token.
  // In the lm-layer's current form, this should only ever have one entry.
  // Leaves 'design space' for if/when we add support for phrase-level corrections/predictions.
  searchSpace: SearchSpace[] = [];

  constructor(source: TrackedContextState);
  constructor(model: LexicalModel);
  constructor(obj: TrackedContextState | LexicalModel) {
    if(obj instanceof TrackedContextState) {
      let source = obj;
      // Be sure to deep-copy the tokens!  Pointer-aliasing is bad here.
      this.tokens = source.tokens.map(function(token) {
        let copy = new TrackedContextToken();
        Object.assign(copy, token);
        return copy;
      });

      this.indexOffset = 0;
      const lexicalModel = this.model = obj.model;
      this.taggedContext = obj.taggedContext;

      if(lexicalModel?.traverseFromRoot) {
        // We need to construct a separate search space from other ContextStates.
        //
        // In case we are unable to perfectly track context (say, due to multitaps)
        // we need to ensure that only fully-utilized keystrokes are considered.
        this.searchSpace = obj.searchSpace.map((space) => new SearchSpace(space));
      }
    } else {
      let lexicalModel = obj;
      this.tokens = [];
      this.indexOffset = Number.MIN_SAFE_INTEGER;
      this.model = lexicalModel;

      if(lexicalModel && lexicalModel.traverseFromRoot) {
        this.searchSpace = [new SearchSpace(lexicalModel)];
      }
    }
  }

  get head(): TrackedContextToken {
    return this.tokens[0];
  }

  get tail(): TrackedContextToken {
    return this.tokens[this.tokens.length - 1];
  }

  popHead() {
    this.tokens.splice(0, 1);
    this.indexOffset -= 1;
  }

  pushTail(token: TrackedContextToken) {
    if(this.model && this.model.traverseFromRoot) {
      this.searchSpace = [new SearchSpace(this.model)]; // yeah, need to update SearchSpace for compatibility
    } else {
      this.searchSpace = [];
    }
    this.tokens.push(token);

    let state = this;
    if(state.searchSpace.length > 0) {
      token.transformDistributions.forEach(distrib => state.searchSpace[0].addInput(distrib));
    }
  }

  toRawTokenization() {
    let sequence: USVString[] = [];

    for(let token of this.tokens) {
      // Hide any tokens representing wordbreaks.  (Thinking ahead to phrase-level possibilities)
      if(token.currentText !== null) {
        sequence.push(token.currentText);
      }
    }

    return sequence;
  }
}

class CircularArray<Item> {
  static readonly DEFAULT_ARRAY_SIZE = 5;
  private circle: Item[];
  private currentHead: number=0;
  private currentTail: number=0;

  constructor(size: number = CircularArray.DEFAULT_ARRAY_SIZE) {
    this.circle = Array(size);
  }

  get count(): number {
    let diff = this.currentHead - this.currentTail;

    if(diff < 0) {
      diff = diff + this.circle.length;
    }

    return diff;
  }

  get maxCount(): number {
    return this.circle.length;
  }

  get oldest(): Item {
    if(this.count == 0) {
      return undefined;
    }

    return this.item(0);
  }

  get newest(): Item {
    if(this.count == 0) {
      return undefined;
    }

    return this.item(this.count - 1);
  }

  enqueue(item: Item): Item {
    var prevItem = null;
    let nextHead = (this.currentHead + 1) % this.maxCount;

    if(nextHead == this.currentTail) {
      prevItem = this.circle[this.currentTail];
      this.currentTail = (this.currentTail + 1) % this.maxCount;
    }

    this.circle[this.currentHead] = item;
    this.currentHead = nextHead;

    return prevItem;
  }

  dequeue(): Item {
    if(this.currentTail == this.currentHead) {
      return null;
    } else {
      let item = this.circle[this.currentTail];
      this.currentTail = (this.currentTail + 1) % this.maxCount;
      return item;
    }
  }

  popNewest(): Item {
    if(this.currentTail == this.currentHead) {
      return null;
    } else {
      let item = this.circle[this.currentHead];
      this.currentHead = (this.currentHead - 1 + this.maxCount) % this.maxCount;
      return item;
    }
  }

  /**
   * Returns items contained within the circular array, ordered from 'oldest' to 'newest' -
   * the same order in which the items will be dequeued.
   * @param index
   */
  item(index: number) {
    if(index >= this.count) {
      // JS arrays return `undefined` for invalid array indices.
      return undefined;
    }

    let mappedIndex = (this.currentTail + index) % this.maxCount;
    return this.circle[mappedIndex];
  }
}

interface ContextMatchResult {
  /**
   * Represents the current state of the context after applying incoming keystroke data.
   */
  state: TrackedContextState;

  /**
   * Represents the previously-cached context state that best matches `state` if available.
   * May be `null` if no such state could be found within the context-state cache.
   */
  baseState: TrackedContextState;

  /**
   * Indicates the portion of the incoming keystroke data, if any, that applies to
   * tokens before the last pre-caret token and thus should not be replaced by predictions
   * based upon `state`.  If the provided context state + the incoming transform do not 
   * adequately match the current context, the match attempt will fail with a `null` result.
   *
   * Should generally be non-null if the token before the caret did not previously exist.
   * 
   * The result may be null if it does not match the prior context state or if bookkeeping 
   * based upon it is problematic - say, if wordbreaking effects shift due to new input, 
   * causing a mismatch with the prior state's tokenization.  
   * (Refer to #12494 for an example case.)
   */
  preservationTransform?: Transform;

  headTokensRemoved: number;
  tailTokensAdded: number;
}

export class ContextTracker extends CircularArray<TrackedContextState> {
  static attemptMatchContext(
    tokenizedContext: Token[],
    matchState: TrackedContextState,
    transformSequenceDistribution?: Distribution<Transform[]>
  ): ContextMatchResult {
    // Map the previous tokenized state to an edit-distance friendly version.
    let matchContext: USVString[] = matchState.toRawTokenization();

    // Inverted order, since 'match' existed before our new context.
    let mapping = ClassicalDistanceCalculation.computeDistance(
      matchContext.map(value => ({key: value})),
      tokenizedContext.map(value => ({key: value.text})),
      // Must be at least 2, as adding a single whitespace after a token tends
      // to add two tokens: one for whitespace, one for the empty token to
      // follow it.
      3
    );

    let editPath = mapping.editPath();
    const firstMatch = editPath.indexOf('match');
    const lastMatch = getEditPathLastMatch(editPath);

    // Assertion:  for a long context, the bulk of the edit path should be a
    // continuous block of 'match' entries.  If there's anything else in
    // the middle, we have a context mismatch.
    if(firstMatch) {
      for(let i = firstMatch+1; i < lastMatch; i++) {
        if(editPath[i] != 'match') {
          return null;
        }
      }
    }

    // If we have a perfect match with a pre-existing context, no mutations have
    // happened; just re-use the old context state.
    if(firstMatch == 0 && lastMatch == editPath.length - 1) {
      return { state: matchState, baseState: matchState, headTokensRemoved: 0, tailTokensAdded: 0 };
    }

    // If mutations HAVE happened, we have work to do.
    let state = matchState;

    let priorEdit: typeof editPath[0];
    let poppedTokenCount = 0;
    for(let i = 0; i < firstMatch; i++) {
      switch(editPath[i]) {
        case 'delete':
          if(priorEdit && priorEdit != 'delete') {
            return null;
          }
          if(state == matchState) {
            state = new TrackedContextState(state);
          }
          state.popHead();
          poppedTokenCount++;
          break;
        case 'substitute':
          // There's no major need to drop parts of a token being 'slid' out of the context window.
          // We'll leave it intact.
          break;
        default:
          // No 'insert' should exist on the leading edge of context when the
          // context window slides.
          //
          // No 'transform' edits should exist within this section, either.
          return null;
      }
    }

    const hasDistribution = transformSequenceDistribution && Array.isArray(transformSequenceDistribution);

    // Reset priorEdit for the end-of-context updating loop.
    priorEdit = undefined;

    // Used to construct and represent the part of the incoming transform that
    // does not land as part of the final token in the resulting context.  This
    // component should be preserved by any suggestions that get applied.
    let preservationTransform: Transform;
    let pushedTokenCount = 0;

    // Now to update the end of the context window.
    for(let i = lastMatch+1; i < editPath.length; i++) {
      const isLastToken = i == editPath.length - 1;

      // If we didn't get any input, we really should perfectly match
      // a previous context state.  If such a state is out of our cache,
      // it should simply be rebuilt.
      if(!hasDistribution) {
        return null;
      }
      const transformDistIndex = i - (lastMatch + 1);
      const tokenDistribution = transformSequenceDistribution.map((entry) => {
        const sample = entry.sample[transformDistIndex];
        if(!sample) {
          return null;
        }
        return {
          sample,
          p: entry.p
        };
      });

      const incomingToken = tokenizedContext[i - poppedTokenCount];

      // If the tokenized part of the input is a completely empty transform,
      // replace it with null.  This can happen with our default wordbreaker
      // immediately after a whitespace.  We don't want to include this
      // transform as part of the input when doing correction-search.
      let primaryInput = hasDistribution ? tokenDistribution[0]?.sample : null;

      // If the incoming token has text but we have no transform (or 'insert') to match
      // it with, abort the matching attempt.  We can't match this case well yet.
      if(editPath[i] != 'delete') {
        if(!incomingToken) {
          return null;
        } else if(!(primaryInput || editPath[i] == 'insert' ) && incomingToken?.text != '') {
          return null;
        }
      }

      if(primaryInput && primaryInput.insert == "" && primaryInput.deleteLeft == 0 && !primaryInput.deleteRight) {
        primaryInput = null;
      }

      // If this token's transform component is not part of the final token,
      // it's something we'll want to preserve even when applying suggestions
      // for the final token.
      //
      // Note:  will need a either a different approach or more specialized
      // handling if/when supporting phrase-level (multi-token) suggestions.
      if(!isLastToken) {
        preservationTransform = preservationTransform && primaryInput ? buildMergedTransform(preservationTransform, primaryInput) : (primaryInput ?? preservationTransform);
      }
      const isBackspace = primaryInput && TransformUtils.isBackspace(primaryInput);

      switch(editPath[i]) {
        case 'substitute':
          if(isLastToken) {
            state = new TrackedContextState(state);
          }

          const sourceToken = matchState.tokens[i];
          state.tokens[i - poppedTokenCount] = sourceToken;
          const token = state.tokens[i - poppedTokenCount];

          // TODO:  I'm beginning to believe that searchSpace should (eventually) be tracked
          // on the tokens, rather than on the overall 'state'.
          // - Reason:  phrase-level corrections / predictions would likely need a search-state
          //   across per potentially-affected token.
          // - Shifting the paradigm should be a separate work unit than the
          //   context-tracker rework currently being done, though.
          if(isBackspace) {
            token.updateWithBackspace(incomingToken.text, primaryInput.id);
            if(isLastToken) {
              state.tokens.pop(); // pops `token`
              // puts it back in, rebuilding a fresh search-space that uses the rebuilt
              // keystroke distribution from updateWithBackspace.
              state.pushTail(token);
            }
          } else {
            token.update(
              tokenDistribution,
              incomingToken.text
            );

            if(isLastToken) {
              // Search spaces may not exist during some unit tests; the state
              // may not have an associated model during some.
              state.searchSpace[0]?.addInput(tokenDistribution);
            }
          }

          // For this case, we were _likely_ called by
          // ModelCompositor.acceptSuggestion(), which would have marked the
          // accepted suggestion.
          //
          // Upon inspection, this doesn't seem entirely ideal.  It works for
          // the common case, but not for specially crafted keystroke
          // transforms.  That said, it's also very low impact.  Best as I can
          // see, this is only really used for debugging info?
          if(state != matchState && !isLastToken) {
            token.replacementText = incomingToken.text;
          }

          break;
        case 'insert':
          if(priorEdit && priorEdit != 'substitute' && priorEdit != 'match' && priorEdit != 'insert') {
            return null;
          }

          if(!preservationTransform) {
            // Allows for consistent handling of "insert" cases; even if there's no edit
            // from a prior token, having a defined transform here indicates that
            // a new token has been produced.  This serves as a useful conditional flag
            // for prediction logic.
            preservationTransform = { insert: '', deleteLeft: 0 };
          }

          if(state == matchState) {
            state = new TrackedContextState(state);
          }

          let pushedToken = new TrackedContextToken();
          pushedToken.raw = incomingToken.text;

          // TODO:  assumes that there was no shift in wordbreaking from the
          // prior context to the current one.  This may actually be a major
          // issue for dictionary-based wordbreaking!
          //
          // If there was such a shift, then we may have extra transforms
          // originally on a 'previous' token that got moved into this one!
          //
          // Suppose we're using a dictionary-based wordbreaker and have
          // `butterfl` for our context, which could become butterfly.  If the
          // next keystroke results in `butterfli`, this would likely be
          // tokenized `butter` `fli`.  (e.g: `fli` leads to `flight`.) How do
          // we know to properly relocate the `f` and `l` transforms?
          if(primaryInput) {
            pushedToken.transformDistributions = tokenDistribution ? [tokenDistribution] : [];
          } else if(incomingToken.text) {
            // We have no transform data to match against an inserted token with text; abort!
            // Refer to #12494 for an example case; we currently can't map previously-committed
            // input transforms to a newly split-off token.
            return null;
          }
          pushedToken.isWhitespace = incomingToken.isWhitespace;

          // Auto-replaces the search space to correspond with the new token.
          state.pushTail(pushedToken);
          pushedTokenCount++;
          break;
        case 'match':
          // The default (Unicode) wordbreaker returns an empty token after whitespace blocks.
          // Adding new whitespace extends the whitespace block but preserves the empty token
          // following it.
          if(priorEdit == 'substitute' && tokenizedContext[tokenizedContext.length-1].text == '') {
            // Keep the blank token as-is; no edit needed!
            continue;
          }
          // else 'fallthrough' / return null
        case 'delete':
          // While we do keep a cache of recent contexts, logic constraints for handling
          // multitaps makes it tricky to reliably use in all situations.
          // It's best to handle `delete` cases directly for this reason.
          for(let j = i + 1; j < editPath.length; j++) {
            // If something _other_ than delete follows a 'delete' on the edit path, 
            // we probably have a context mismatch.
            //
            // It's possible to construct cases where this isn't true, but it's likely not
            // worth trying to handle such rare cases.
            if(editPath[j] != 'delete') {
              return null;
            }
          }

          // If ALL that remains are deletes, we're good to go.
          //
          // This may not be the token at the index, but since all that remains are deletes,
          // we'll have deleted the correct total number from the end once all iterations
          // are done.
          if(state == matchState) {
            state = new TrackedContextState(state);
          }

          state.tokens.pop();
          break;
        default:
          // No 'transform' edits should exist within this section.
          return null;
      }

      priorEdit = editPath[i];
    }

    return { 
      state, 
      baseState: matchState, 
      preservationTransform, 
      headTokensRemoved: poppedTokenCount, 
      tailTokensAdded: pushedTokenCount 
    };
  }

  private static modelContextState(
    tokenizedContext: Token[],
    lexicalModel: LexicalModel
  ): TrackedContextState {
    let baseTokens = tokenizedContext.map(function(entry) {
      let token = new TrackedContextToken();
      token.raw = entry.text;
      if(entry.isWhitespace) {
        token.isWhitespace = true;
      }

      if(token.raw) {
        token.transformDistributions = textToCharTransforms(token.raw).map(function(transform) {
          return [{sample: transform, p: 1.0}];
        });
      } else {
        // Helps model context-final wordbreaks.
        token.transformDistributions = [];
      }
      return token;
    });

    // And now build the final context state object, which includes whitespace 'tokens'.
    let state = new TrackedContextState(lexicalModel);

    while(baseTokens.length > 0) {
      // We don't have a pre-existing distribution for this token, so we'll build one as
      // if we'd just produced the token from a backspace.
      if(baseTokens.length == 1) {
        baseTokens[0].updateWithBackspace(baseTokens[0].raw, null);
      }
      state.pushTail(baseTokens.splice(0, 1)[0]);
    }

    if(state.tokens.length == 0) {
      let token = new TrackedContextToken();
      token.raw = '';

      state.pushTail(token);
    }

    return state;
  }

  /**
   * Compares the current, post-input context against the most recently-seen contexts from previous prediction calls, returning
   * the most information-rich `TrackedContextState` possible.  If a match is found, the state will be annotated with the
   * input information provided to previous prediction calls and persisted correction-search calculations for re-use.
   *
   * @param model
   * @param context
   * @param transformDistribution
   * @param preserveMatchState  Set to `true` to avoid any edits to the matched context state when they might normally occur.
   */
  analyzeState(
    model: LexicalModel,
    context: Context,
    transformDistribution?: Distribution<Transform>,
    preserveMatchState?: boolean
  ): ContextMatchResult {
    if(!model.traverseFromRoot) {
      // Assumption:  LexicalModel provides a valid traverseFromRoot function.  (Is technically optional)
      // Without it, no 'corrections' may be made; the model can only be used to predict, not correct.
      throw "This lexical model does not provide adequate data for correction algorithms and context reuse";
    }

    let tokenize = determineModelTokenizer(model);

    const inputTransform = transformDistribution?.[0];
    let transformTokenLength = 0;
    let tokenizedDistribution: Distribution<Transform[]> = null;
    if(inputTransform) {
      // These two methods apply transforms internally; do not mutate context here.
      // This particularly matters for the 'distribution' variant.
      transformTokenLength = tokenizeTransform(tokenize, context, inputTransform.sample).length;
      tokenizedDistribution = tokenizeTransformDistribution(tokenize, context, transformDistribution);

      // Now we update the context used for context-state management based upon our input.
      context = applyTransform(inputTransform.sample, context);

      // While we lack phrase-based / phrase-oriented prediction support, we'll just extract the
      // set that matches the token length that results from our input.
      tokenizedDistribution = tokenizedDistribution.filter((entry) => entry.sample.length == transformTokenLength);
    }

    const tokenizedContext = tokenize(context);

    if(tokenizedContext.left.length > 0) {
      for(let i = this.count - 1; i >= 0; i--) {
        const priorMatchState = this.item(i);

        // Skip intermediate multitap-produced contexts.
        // When multitapping, we skip all contexts from prior taps within the same interaction,
        // but not any contexts from before the multitap started.
        const priorTaggedContext = priorMatchState.taggedContext;
        if(priorTaggedContext && transformDistribution && transformDistribution.length > 0) {
          // Using the potential `matchState` + the incoming transform, do the results line up for
          // our observed context?  If not, skip it.
          //
          // Necessary to properly handle multitaps, as there are context rewinds that the
          // predictive-text engine is not otherwise warned about.
          //
          // `priorTaggedContext` must not be `null`!
          const doublecheckContext = applyTransform(transformDistribution[0].sample, priorTaggedContext);
          if(doublecheckContext.left != context.left) {
            continue;
          }
        } else if(priorTaggedContext?.left != context.left) {
          continue;
        }

        let result = ContextTracker.attemptMatchContext(tokenizedContext.left, this.item(i), tokenizedDistribution);

        if(result?.state) {
          // Keep it reasonably current!  And it's probably fine to have it more than once
          // in the history.  However, if it's the most current already, there's no need
          // to refresh it.
          if(this.newest != result.state && this.newest != priorMatchState) {
            // Already has a taggedContext.
            this.enqueue(priorMatchState);
          }

          result.state.taggedContext = context;
          if(result.state != this.item(i)) {
            this.enqueue(result.state);
          }
          return result;
        }
      }
    }

    // Else:  either empty OR we've detected a 'new context'.  Initialize from scratch; no prior input information is
    // available.  Only the results of the prior inputs are known.
    //
    // Assumption:  as a caret needs to move to context before any actual transform distributions occur,
    // this state is only reached on caret moves; thus, transformDistribution is actually just a single null transform.
    let state = ContextTracker.modelContextState(tokenizedContext.left, model);
    state.taggedContext = context;
    this.enqueue(state);
    return { state, baseState: null, headTokensRemoved: 0, tailTokensAdded: 0 };
  }

  clearCache() {
    while(this.count > 0) {
      this.dequeue();
    }
  }
}
