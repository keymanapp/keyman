/// <reference path="distance-modeler.ts" />

namespace correction {
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

    get isNew(): boolean {
      return this.transformDistributions.length == 0;
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

    revert() {
      delete this.activeReplacementId;
    }
  }

  export class TrackedContextState {
    // Stores the source Context (as a debugging reference).  Not currently utilized.
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
          copy.replacements = token.replacements
          copy.activeReplacementId = token.activeReplacementId;
          copy.transformDistributions = token.transformDistributions;

          if(token.replacementText) {
            copy.replacementText = token.replacementText;
          }
  
          return copy;
        });
        this.searchSpace = obj.searchSpace;
        this.indexOffset = 0;
        this.model = obj.model;
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
      whitespaceToken.transformDistributions = [transformDistribution]; 
      
      whitespaceToken.raw = null;
      this.tokens.push(whitespaceToken);
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
                               transformDistribution?: Distribution<Transform>,): TrackedContextState {
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
        // Since we're continuing a previously-cached context, we can reuse the same SearchSpace
        // to continue making predictions.
        state = matchState;
      }

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
      if(editPath.length > 1) {
        if(poppedHead) {
          state.popHead();
        }

        if(pushedTail) {
          // ASSUMPTION:  any transform that triggers this case is a pure-whitespace Transform, as we
          //              need a word-break before beginning a new word's context.
          //              Worth note:  when invalid, the lm-layer already has problems in other aspects too.
          state.pushWhitespaceToTail(transformDistribution);

          let emptyToken = new TrackedContextToken();
          emptyToken.raw = '';
          // Continuing the earlier assumption, that 'pure-whitespace Transform' does not emit any initial characters
          // for the new word (token), so the input keystrokes do not correspond to the new text token.
          emptyToken.transformDistributions = [];
          state.pushTail(emptyToken);
        } else {
          // TODO:  Assumption:  we didn't 'miss' any inputs somehow.
          //        As is, may be prone to fragility should the lm-layer's tracked context 'desync' from its host's.
          state.updateTail(transformDistribution, tokenizedContext[tokenizedContext.length-1]);
        }
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
          state.updateTail(transformDistribution, tokenizedContext[0]);
        }
      }
      return state;
    }

    static modelContextState(tokenizedContext: USVString[], lexicalModel: LexicalModel): TrackedContextState {
      let baseTokens = tokenizedContext.map(function(entry) {
        let token = new TrackedContextToken();
        token.raw = entry;
        if(token.raw) {
          let tokenTransform = {
            insert: entry,
            deleteLeft: 0
          };
          // Build a single-entry prob-distribution array... where the single distribution is 100% for the token's actual form.
          // Basically, assume the token was the correct input, since we lack any actual probability data about the keystrokes
          // that generated it.
          token.transformDistributions = [[{
            sample: tokenTransform,
            p: 1.0
          }]];
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
     * @param mainTransform 
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

      let tokenizedContext = models.tokenize(model.wordbreaker || wordBreakers.default, context);

      if(tokenizedContext.left.length > 0) {
        for(let i = this.count - 1; i >= 0; i--) {
          let resultState = ContextTracker.attemptMatchContext(tokenizedContext.left, this.item(i), transformDistribution);

          if(resultState) {
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
  }
}
