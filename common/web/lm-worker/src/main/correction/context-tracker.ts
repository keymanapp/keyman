import { applyTransform, tokenize } from '@keymanapp/models-templates';
import { defaultWordbreaker } from '@keymanapp/models-wordbreakers';

import { ClassicalDistanceCalculation } from './classical-calculation.js';
import { SearchSpace } from './distance-modeler.js';
import TransformUtils from '../transformUtils.js';

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

export class TrackedContextSuggestion {
  suggestion: Suggestion;
  tokenWidth: number;
}

export class TrackedContextToken {
  raw: string;
  replacementText: string;

  transformDistributions: Distribution<Transform>[] = [];
  replacements: TrackedContextSuggestion[];
  activeReplacementId: number = -1;

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

  revert() {
    delete this.activeReplacementId;
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
        copy.raw = token.raw;
        copy.replacements = [].concat(token.replacements);
        copy.activeReplacementId = token.activeReplacementId;
        copy.transformDistributions = [].concat(token.transformDistributions);

        if(token.replacementText) {
          copy.replacementText = token.replacementText;
        }

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
    this.tokens.splice(0, 2);
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

  pushWhitespaceToTail(transformDistribution: Distribution<Transform> = null) {
    let whitespaceToken = new TrackedContextToken();

    // Track the Transform that resulted in the whitespace 'token'.
    // Will be needed for phrase-level correction/prediction.
    whitespaceToken.transformDistributions = transformDistribution ? [transformDistribution] : [];

    whitespaceToken.raw = null;
    this.tokens.push(whitespaceToken);
  }

  /**
   * Used for 14.0's backspace workaround, which flattens all previous Distribution<Transform>
   * entries because of limitations with direct use of backspace transforms.
   * @param tokenText
   * @param transformId
   */
  replaceTailForBackspace(tokenText: USVString, transformId: number) {
    this.tokens.pop();

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

    let compactedToken = new TrackedContextToken();
    compactedToken.raw = tokenText;
    compactedToken.transformDistributions = backspacedTokenContext;
    this.pushTail(compactedToken);
  }

  updateTail(transformDistribution: Distribution<Transform>, tokenText?: USVString) {
    let editedToken = this.tail;

    // Preserve existing text if new text isn't specified.
    tokenText = tokenText || (tokenText === '' ? '' : editedToken.raw);

    if(transformDistribution && transformDistribution.length > 0) {
      editedToken.transformDistributions.push(transformDistribution);
      if(this.searchSpace) {
        this.searchSpace.forEach(space => space.addInput(transformDistribution));
      }
    }
    // Replace old token's raw-text with new token's raw-text.
    editedToken.raw = tokenText;
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
      throw "Invalid array index";
    }

    let mappedIndex = (this.currentTail + index) % this.maxCount;
    return this.circle[mappedIndex];
  }
}

export class ContextTracker extends CircularArray<TrackedContextState> {
  static attemptMatchContext(tokenizedContext: USVString[],
                              matchState: TrackedContextState,
                              transformDistribution?: Distribution<Transform>): TrackedContextState {
    // Map the previous tokenized state to an edit-distance friendly version.
    let matchContext: USVString[] = matchState.toRawTokenization();

    // Inverted order, since 'match' existed before our new context.
    let mapping = ClassicalDistanceCalculation.computeDistance(matchContext.map(value => ({key: value})),
                                                                tokenizedContext.map(value => ({key: value})),
                                                                1);

    let editPath = mapping.editPath();

    let poppedHead = false;
    let pushedTail = false;

    // Matters greatly when starting from a nil context.
    if(editPath.length > 1) {
      // First entry:  may not be an 'insert' or a 'transpose' op.
      // 'insert' allowed if the next token is 'substitute', as this may occur with an edit path of length 2.
      if((editPath[0] == 'insert' && !(editPath[1] == 'substitute' && editPath.length == 2)) || editPath[0].indexOf('transpose') >= 0) {
        return null;
      } else if(editPath[0] == 'delete') {
        poppedHead = true; // a token from the previous state has been wholly removed.
      }
    }

    // Last entry:  may not be a 'delete' or a 'transpose' op.
    let tailIndex = editPath.length -1;
    let ignorePenultimateMatch = false;
    if(editPath[tailIndex] == 'delete' || editPath[0].indexOf('transpose') >= 0) {
      return null;
    } else if(editPath[tailIndex] == 'insert') {
      pushedTail = true;
    } else if(tailIndex > 0 && editPath[tailIndex-1] == 'insert' && editPath[tailIndex] == 'substitute') {
      // Tends to happen when accepting suggestions.
      pushedTail = true;
      ignorePenultimateMatch = true;
    }

    // Can happen for the first text input after backspace deletes a wordbreaking character,
    // thus the new input continues a previous word while dropping the empty word after
    // that prior wordbreaking character.
    //
    // We can't handle it reliably from this match state, but a previous entry (without the empty token)
    // should still be in the cache and will be reliable for this example case.
    if(tailIndex > 0 && editPath[tailIndex-1] == 'delete' && editPath[tailIndex] == 'substitute') {
      return null;
    }

    // Now to check everything in-between:  should be exclusively 'match'es.
    for(let index = 1; index < editPath.length - (ignorePenultimateMatch ? 2 : 1); index++) {
      if(editPath[index] != 'match') {
        return null;
      }
    }

    // If we've made it here... success!  We have a context match!
    let state: TrackedContextState;

    if(pushedTail) {
      // On suggestion acceptance, we should update the previous final token.
      // We do it first so that the acceptance is replicated in the new TrackedContextState
      // as well.
      if(ignorePenultimateMatch) {
        // For this case, we were likely called by ModelCompositor.acceptSuggestion(), which
        // would have marked the accepted suggestion.
        matchState.tail.replacementText = tokenizedContext[tokenizedContext.length-2];
      }

      state = new TrackedContextState(matchState);
    } else {
      // We're continuing a previously-cached context; create a deep-copy of it.
      // We can't just re-use the old instance, unfortunately; predictions break
      // with multitaps otherwise - we should avoid tracking keystrokes that were
      // rewound.
      //
      // If there are no incoming transforms, though... yeah, re-use is safe then.
      state = !!transformDistribution ? new TrackedContextState(matchState) : matchState;
    }

    const hasDistribution = transformDistribution && Array.isArray(transformDistribution);
    let primaryInput = hasDistribution ? transformDistribution[0].sample : null;
    if(primaryInput && primaryInput.insert == "" && primaryInput.deleteLeft == 0 && !primaryInput.deleteRight) {
      primaryInput = null;
    }

    const isWhitespace = primaryInput && TransformUtils.isWhitespace(primaryInput);
    const isBackspace = primaryInput && TransformUtils.isBackspace(primaryInput);
    const finalToken = tokenizedContext[tokenizedContext.length-1];

    /* Assumption:  This is an adequate check for its two sub-branches.
      *
      * Basis:
      * - Assumption: one keystroke may only cause a single token to rotate out of context.
      *   - That is, no "reasonable" keystroke would emit enough code points to 'bump' two words simultaneously.
      *   - ... This one may need to be loosened a bit... but it should be enough for initial correction testing as-is.
      * - Assumption:  one keystroke may only cause a single token to be appended to the context
      *   - That is, no "reasonable" keystroke would emit a Transform adding two separate word tokens
      *     - For languages using whitespace to word-break, said keystroke would have to include said whitespace to break the assumption.
      */

    function maintainLastToken() {
      if(isWhitespace && editPath[tailIndex] == 'match') {
        /*
          We can land here if there are multiple whitespaces in a row.
          There's already an implied whitespace to the left, so we conceptually
          merge the new whitespace with that one.
        */
        return state;
      } else if(isBackspace) {
        // Consider backspace entry for this case?
        state.replaceTailForBackspace(finalToken, primaryInput.id);
      } else {
        state.updateTail(primaryInput ? transformDistribution : null, finalToken);
      }
    }

    // If there is/was more than one context token available...
    if(editPath.length > 1) {
      // We're removing a context token, but at least one remains.
      if(poppedHead) {
        state.popHead();
      }

      // We're adding an additional context token.
      if(pushedTail) {
        const tokenizedTail = tokenizedContext[tokenizedContext.length - 1];
        /*
          * Common-case:  most transforms that trigger this case are from pure-whitespace Transforms.  MOST.
          *
          * Less-common, but noteworthy:  some wordbreaks may occur without whitespace.  Example:
          * `"o` => ['"', 'o'].  Make sure to double-check against `tokenizedContext`!
          */
        let pushedToken = new TrackedContextToken();
        pushedToken.raw = tokenizedTail;

        if(isWhitespace || !primaryInput) {
          state.pushWhitespaceToTail(transformDistribution ?? []);
          // Continuing the earlier assumption, that 'pure-whitespace Transform' does not emit any initial characters
          // for the new word (token), so the input keystrokes do not correspond to the new text token.
          pushedToken.transformDistributions = [];
        } else {
          state.pushWhitespaceToTail();
          // Assumption: Since we only allow one-transform-at-a-time changes between states, we shouldn't be missing
          // any metadata used to construct the new context state token.
          pushedToken.transformDistributions = transformDistribution ? [transformDistribution] : [];
        }

        state.pushTail(pushedToken);
      } else {
        // We're editing the final context token.
        // TODO:  Assumption:  we didn't 'miss' any inputs somehow.
        //        As is, may be prone to fragility should the lm-layer's tracked context 'desync' from its host's.
        maintainLastToken();
      }
      // There is only one word in the context.
    } else {
      // TODO:  Assumption:  we didn't 'miss' any inputs somehow.
      //        As is, may be prone to fragility should the lm-layer's tracked context 'desync' from its host's.

      if(editPath[tailIndex] == 'insert') {
        // Construct appropriate initial token.
        let token = new TrackedContextToken();
        token.raw = tokenizedContext[0];
        token.transformDistributions = [transformDistribution];
        state.pushTail(token);
      } else {
        // Edit the lone context token.
        maintainLastToken();
      }
    }
    return state;
  }

  private static modelContextState(tokenizedContext: USVString[],
                            transformDistribution: Distribution<Transform>,
                            lexicalModel: LexicalModel): TrackedContextState {
    let baseTokens = tokenizedContext.map(function(entry) {
      let token = new TrackedContextToken();
      token.raw = entry;
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

    if(baseTokens.length > 0) {
      state.pushTail(baseTokens.splice(0, 1)[0]);
    }

    while(baseTokens.length > 0) {
      state.pushWhitespaceToTail();
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
   */
  analyzeState(model: LexicalModel,
                context: Context,
                transformDistribution?: Distribution<Transform>): TrackedContextState {
    if(!model.traverseFromRoot) {
      // Assumption:  LexicalModel provides a valid traverseFromRoot function.  (Is technically optional)
      // Without it, no 'corrections' may be made; the model can only be used to predict, not correct.
      throw "This lexical model does not provide adequate data for correction algorithms and context reuse";
    }

    let tokenizedContext = tokenize(model.wordbreaker || defaultWordbreaker, context);

    if(tokenizedContext.left.length > 0) {
      for(let i = this.count - 1; i >= 0; i--) {
        const priorMatchState = this.item(i);
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

        let resultState = ContextTracker.attemptMatchContext(tokenizedContext.left, this.item(i), transformDistribution);

        if(resultState) {
          // Keep it reasonably current!  And it's probably fine to have it more than once
          // in the history.  However, if it's the most current already, there's no need
          // to refresh it.
          if(this.newest != resultState && this.newest != priorMatchState) {
            // Already has a taggedContext.
            this.enqueue(priorMatchState);
          }

          resultState.taggedContext = context;
          if(resultState != this.item(i)) {
            this.enqueue(resultState);
          }
          return resultState;
        }
      }
    }

    // Else:  either empty OR we've detected a 'new context'.  Initialize from scratch; no prior input information is
    // available.  Only the results of the prior inputs are known.
    //
    // Assumption:  as a caret needs to move to context before any actual transform distributions occur,
    // this state is only reached on caret moves; thus, transformDistribution is actually just a single null transform.
    let state = ContextTracker.modelContextState(tokenizedContext.left, transformDistribution, model);
    state.taggedContext = context;
    this.enqueue(state);
    return state;
  }

  clearCache() {
    while(this.count > 0) {
      this.dequeue();
    }
  }
}
