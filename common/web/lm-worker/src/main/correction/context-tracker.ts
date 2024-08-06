import { Token, applyTransform } from '@keymanapp/models-templates';

import { ClassicalDistanceCalculation } from './classical-calculation.js';
import { SearchSpace } from './distance-modeler.js';
import TransformUtils from '../transformUtils.js';
import { determineModelTokenizer } from '../model-helpers.js';

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
  isWhitespace?: boolean;

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

  updateToken(token: TrackedContextToken, transformDistribution: Distribution<Transform>, tokenText?: USVString) {
    // Preserve existing text if new text isn't specified.
    tokenText = tokenText || (tokenText === '' ? '' : token.raw);

    if(transformDistribution && transformDistribution.length > 0) {
      token.transformDistributions.push(transformDistribution);
      if(this.searchSpace) {
        this.searchSpace.forEach(space => space.addInput(transformDistribution));
      }
    }
    // Replace old token's raw-text with new token's raw-text.
    token.raw = tokenText;
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

export class ContextTracker extends CircularArray<TrackedContextState> {
  static attemptMatchContext(
    tokenizedContext: Token[],
    matchState: TrackedContextState,
    transformDistribution?: Distribution<Transform>
  ): TrackedContextState {
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

    // When the context has but two tokens, the path algorithm tends to invert
    // 'insert' and 'substitute' from our preferred ordering for them.
    // Logically, either order makes sense... but logic for other cases is
    // far simpler if we have 'substitute' before 'insert'.
    if(editPath.length == 2 && editPath[0] == 'insert' && editPath[1] == 'substitute') {
      editPath[0] = 'substitute';
      editPath[1] = 'insert';
    }

    const firstMatch = editPath.indexOf('match');
    const lastMatch = editPath.lastIndexOf('match');

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
      return matchState;
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

    const hasDistribution = transformDistribution && Array.isArray(transformDistribution);
    let primaryInput = hasDistribution ? transformDistribution[0].sample : null;
    if(primaryInput && primaryInput.insert == "" && primaryInput.deleteLeft == 0 && !primaryInput.deleteRight) {
      primaryInput = null;
    }

    // TODO:  "wordbreak" the `insert` section of the transform (if it exists).
    // ... wait, might have to be done at a higher level...
    // ... and will probably want its own unit test ...

    const isBackspace = primaryInput && TransformUtils.isBackspace(primaryInput);

    // Reset priorEdit for the end-of-context updating loop.
    priorEdit = undefined;

    // Now to update the end of the context window.
    for(let i = lastMatch+1; i < editPath.length; i++) {
      const isLastToken = i == editPath.length - 1;

      const incomingToken = tokenizedContext[i - poppedTokenCount]
      switch(editPath[i]) {
        case 'substitute':
          if(isLastToken) {
            state = new TrackedContextState(state);
          }

          const token = state.tokens[i - poppedTokenCount];
          const matchToken = matchState.tokens[i];

          if(isBackspace) {
            token.updateWithBackspace(incomingToken.text, primaryInput.id);
          } else {
            state.updateToken(token, transformDistribution, incomingToken.text);
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
            matchToken.replacementText = incomingToken.text;
          }
          break;
        case 'insert':
          if(priorEdit && priorEdit != 'substitute' && priorEdit != 'match') {
            return null;
          }

          if(state == matchState) {
            state = new TrackedContextState(state);
          }

          let pushedToken = new TrackedContextToken();
          pushedToken.raw = incomingToken.text;

          // TODO: May need something more complicated if the keystroke's
          // transform triggers a wordbreak _within_ its boundaries (rather than
          // on an edge).  (Probably some way to map the tokenization to the indices
          // within `insert`.)
          pushedToken.transformDistributions = transformDistribution ? [transformDistribution] : [];
          pushedToken.isWhitespace = incomingToken.isWhitespace;

          state.pushTail(pushedToken);
          break;
        default:
          // No 'delete' should exist on the trailing edge of context when the
          // context window slides.  While it can happen due to keystrokes with
          // `deleteLeft`, we keep a cache of recent contexts - an older one will
          // likely match sufficiently.
          // - may see 'delete' followed by 'substitute' in such cases.
          //
          // No 'transform' edits should exist within this section, either.
          return null;
      }
    }

    return state;
  }

  private static modelContextState(
    tokenizedContext: {text: USVString, isWhitespace?: boolean}[],
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

    if(baseTokens.length > 0) {
      state.pushTail(baseTokens.splice(0, 1)[0]);
    }

    while(baseTokens.length > 0) {
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
  analyzeState(
    model: LexicalModel,
    context: Context,
    transformDistribution?: Distribution<Transform>
  ): TrackedContextState {
    if(!model.traverseFromRoot) {
      // Assumption:  LexicalModel provides a valid traverseFromRoot function.  (Is technically optional)
      // Without it, no 'corrections' may be made; the model can only be used to predict, not correct.
      throw "This lexical model does not provide adequate data for correction algorithms and context reuse";
    }

    const inputTransform = transformDistribution?.[0];
    if(inputTransform) {
      context = applyTransform(inputTransform.sample, context);
    }

    const tokenize = determineModelTokenizer(model);
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
    let state = ContextTracker.modelContextState(tokenizedContext.left, model);
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
