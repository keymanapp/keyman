/// <reference path="distance-modeler.ts" />

namespace correction {
  export class TrackedContextSuggestion {
    suggestion: Suggestion;
    tokenWidth: number;
  }

  export class TrackedContextToken {
    raw: string;
    transformDistributions: Distribution<Transform>[] = [];
    replacements: TrackedContextSuggestion;
    activeReplacement: number = -1;
  }

  export class TrackedContextState {
    context: Context;
    tokens: TrackedContextToken[];
    poppedHead: boolean;
    pushedTail: boolean;
    
    // Tracks all search spaces starting at the current token.
    // In the lm-layer's current form, this should only ever have one entry.
    // Leaves 'design space' for if/when we add support for phrase-level corrections/predictions.
    searchSpace: SearchSpace[] = [];

    toRawTokenization() {
      // TODO:  support token replacement (for accepted suggestions).
      let sequence: USVString[] = [];
      
      for(let token of this.tokens) {
        // Hide any tokens representing wordbreaks.  (Thinking ahead to phrase-level possibilities)
        if(token.raw !== null) {
          sequence.push(token.raw);
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
                               rootTraversal: LexiconTraversal,
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
        if(editPath[0] == 'insert' || editPath[0].indexOf('transpose') >= 0) {
          return null;
        } else if(editPath[0] == 'delete') {
          poppedHead = true; // a token from the previous state has been wholly removed.
        }
      }

      // Last entry:  may not be a 'delete' or a 'transpose' op.
      let tailIndex = editPath.length -1;
      if(editPath[tailIndex] == 'delete' || editPath[0].indexOf('transpose') >= 0) {
        return null;
      } else if(editPath[tailIndex] == 'insert') {
        pushedTail = true;
      }

      // Now to check everything in-between:  should be exclusively 'match'es.
      for(let index = 1; index < editPath.length - 2; index++) {
        if(editPath[index] != 'match') {
          return null;
        }
      }

      // If we've made it here... success!  We have a context match!
      let newState = new TrackedContextState();

      // Be sure to deep-copy the tokens!  Pointer-aliasing is bad here.
      newState.tokens = matchState.tokens.map(function(token) {
        let copy = new TrackedContextToken();
        copy.raw = token.raw;
        copy.replacements = token.replacements
        copy.activeReplacement = token.activeReplacement;
        copy.transformDistributions = token.transformDistributions;

        return copy;
      });

      // Since we're continuing a previously-cached context, we can reuse the same SearchSpace
      // to continue making predictions.
      newState.searchSpace = matchState.searchSpace;
      newState.poppedHead = poppedHead;
      newState.pushedTail = pushedTail;

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
          newState.tokens.splice(0, 2);  // Chop off the first token and its subsequent 'whitespace' token.
        }
        if(pushedTail) {
          let whitespaceToken = new TrackedContextToken();

          // ASSUMPTION:  any transform that triggers this case is a pure-whitespace Transform.
          //              Worth note:  when invalid, the lm-layer already has problems in other aspects too.
          whitespaceToken.transformDistributions = [transformDistribution]; // Track the Transform that resulted in the whitespace 'token'.
                                                    // Will be needed for phrase-level correction/prediction.
          
          // Note:  we don't bother 'correcting' whitespace tokens at this time.
          //        They don't get any SearchSpace handling.

          whitespaceToken.raw = null;
          newState.tokens.push(whitespaceToken);

          let emptyToken = new TrackedContextToken();
          emptyToken.raw = '';
          emptyToken.transformDistributions = [];

          // For now... new final token => throw out old SearchSpace, use new SearchSpace.
          if(rootTraversal) {
            newState.searchSpace = [new correction.SearchSpace(rootTraversal)];
          }
          newState.tokens.push(emptyToken);
        } else {
          // TODO:  Assumption:  we didn't 'miss' any inputs somehow.
          //        As is, may be prone to fragility should the lm-layer's tracked context 'desync' from its host's.
          let editedToken = newState.tokens[newState.tokens.length - 1];
          if(transformDistribution && transformDistribution.length > 0) {
            editedToken.transformDistributions.push(transformDistribution);
            if(newState.searchSpace) {
              newState.searchSpace.forEach(space => space.addInput(transformDistribution));
            }
          }
          // Replace old token's raw-text with new token's raw-text.
          editedToken.raw = tokenizedContext[tokenizedContext.length - 1];
        }
      } else {
        // TODO:  Assumption:  we didn't 'miss' any inputs somehow.
        //        As is, may be prone to fragility should the lm-layer's tracked context 'desync' from its host's.
        let editedToken: TrackedContextToken;
        if(editPath[tailIndex] == 'insert') {
          editedToken = new TrackedContextToken();
        } else {
          editedToken = newState.tokens[newState.tokens.length - 1]; 
        }
        if(transformDistribution && transformDistribution.length > 0) {
          editedToken.transformDistributions.push(transformDistribution);
          if(newState.searchSpace) {
            newState.searchSpace.forEach(space => space.addInput(transformDistribution));
          }
        }
        // Replace old token's raw-text with new token's raw-text.
        editedToken.raw = tokenizedContext[tokenizedContext.length - 1];
      }
      return newState;
    }

    static modelContextState(tokenizedContext: USVString[], traversalRoot: LexiconTraversal): TrackedContextState {
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

      // And now to add the whitespace.
      let finalTokens: TrackedContextToken[] = [];

      if(baseTokens.length > 0) {
        finalTokens.push(baseTokens.splice(0, 1)[0]);
      }

      while(baseTokens.length > 0) {
        let whitespaceToken = new TrackedContextToken();
        whitespaceToken.transformDistributions = [];
        whitespaceToken.raw = null;

        finalTokens.push(whitespaceToken);
        finalTokens.push(baseTokens.splice(0, 1)[0]);
      }

      let state = new TrackedContextState();
      state.poppedHead = false;
      state.pushedTail = false;
      state.tokens = finalTokens;
      if(traversalRoot) {
        state.searchSpace = [new SearchSpace(traversalRoot)];
        if(finalTokens.length > 0) {
          state.tokens[state.tokens.length - 1].transformDistributions.forEach(distrib => state.searchSpace[0].addInput(distrib));
        }
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

      let tokenizedContext = model.tokenize(context);

      if(tokenizedContext.length > 0) {
        for(let i = this.count - 1; i >= 0; i--) {
          let resultState = ContextTracker.attemptMatchContext(tokenizedContext, this.item(i), model.traverseFromRoot(), transformDistribution);

          if(resultState) {
            resultState.context = context;
            this.enqueue(resultState);
            return resultState;
          }
        }
      }

      // Else:  either empty OR we've detected a 'new context'.  Initialize from scratch; no prior input information is
      // available.  Only the results of the prior inputs are known.
      //
      // Assumption:  as a caret needs to move to context before any actual transform distributions occur,
      // this state is only reached on caret moves; thus, transformDistribution is actually just a single null transform.
      let state = ContextTracker.modelContextState(tokenizedContext, model.traverseFromRoot());
      state.context = context;
      this.enqueue(state);
      return state;
    }
  }
}