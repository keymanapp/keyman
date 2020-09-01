namespace correction {
  export class TrackedContextSuggestion {
    suggestion: Suggestion;
    tokenWidth: number;
  }

  export class TrackedContextToken {
    raw: string;
    // TODO:  -> inputs:  ProbabilityDistribution[] // 
    transforms: Transform[];
    replacements: TrackedContextSuggestion;
    activeReplacement: number = -1;
  }

  export class TrackedContextState {
    context: Context;
    tokens: TrackedContextToken[];
    poppedHead: boolean;
    pushedTail: boolean;
    
    searchSpace: SearchSpace[] = [];

    toKeyedSequence() {
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

      let mappedIndex = (this.currentTail + index) % this.count;
      return this.circle[mappedIndex];
    }
  }

  export class ContextTracker extends CircularArray<TrackedContextState> {
    static attemptMatchContext(tokenizedContext: USVString[], matchState: TrackedContextState, transform: Transform): TrackedContextState {
      // Map the previous tokenized state to an edit-distance friendly version.
      let matchContext: USVString[] = matchState.toKeyedSequence();

      // Inverted order, since 'match' existed before our new context.
      let mapping = ClassicalDistanceCalculation.computeDistance(matchContext.map(value => ({key: value})),
                                                                 tokenizedContext.map(value => ({key: value})),
                                                                 1);

      let editPath = mapping.editPath();

      let poppedHead = false;
      let pushedTail = false;

      // First entry:  may not be an 'insert' or a 'transpose' op.
      if(editPath[0] == 'insert' || editPath[0].indexOf('transpose') >= 0) {
        return null;
      } else if(editPath[0] == 'delete') {
        poppedHead = true; // a token from the previous state has been wholly removed.
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
      newState.tokens = Array.from(matchState.tokens);

      // TODO:  Is this an adequate check for its two sub-branches?
      if(editPath.length > 1) {
        if(poppedHead) {
          newState.tokens.splice(0, 2);  // Chop off the first token and its subsequent 'whitespace' token.
        }
        if(pushedTail) {
          let whitespaceToken = new TrackedContextToken();

          // ASSUMPTION:  any transform that triggers this case is a pure-whitespace Transform.
          whitespaceToken.transforms = [transform]; // Track the Transform that resulted in the whitespace 'token'.
                                                    // Will be needed for phrase-level correction/prediction.
          whitespaceToken.raw = null;
          newState.tokens.push(whitespaceToken);

          let emptyToken = new TrackedContextToken();
          emptyToken.raw = '';
          emptyToken.transforms = [];
          newState.tokens.push(emptyToken);
        } else {
          let editedToken = newState.tokens[newState.tokens.length - 1];
          editedToken.transforms.push(transform);
          editedToken.raw = models.applyTransform(transform, {left: editedToken.raw, startOfBuffer: false, endOfBuffer: false}).left;
        }
      }
      return newState;
    }

    static modelContextState(tokenizedContext: USVString[]): TrackedContextState {
      let baseTokens = tokenizedContext.map(function(entry) {
        let token = new TrackedContextToken();
        token.raw = entry;
        if(token.raw) {
          token.transforms = [{
            insert: entry,
            deleteLeft: 0
          }];
        } else {
          // Helps model context-final wordbreaks.
          token.transforms = [];
        }
        return token;
      });

      // And now to add the whitespace.
      let finalTokens: TrackedContextToken[] = [];
      finalTokens.push(baseTokens.splice(0, 1)[0]);

      while(baseTokens.length > 0) {
        let whitespaceToken = new TrackedContextToken();
        whitespaceToken.transforms = [];
        whitespaceToken.raw = null;

        finalTokens.push(whitespaceToken);
        finalTokens.push(baseTokens.splice(0, 1)[0]);
      }

      let state = new TrackedContextState();
      state.poppedHead = false;
      state.pushedTail = false;
      state.tokens = finalTokens;

      return state;
    }

    analyzeState(model: LexicalModel, context: Context, transform: Transform): TrackedContextState {
      let tokenizedContext = model.tokenize(context);

      if(tokenizedContext.length > 0) {
        for(let i = this.count - 1; i >= 0; i--) {
          let resultState = ContextTracker.attemptMatchContext(tokenizedContext, this.item[i], transform);

          if(resultState) {
            resultState.context = context;
            // TODO:  assign SearchSpace (used for any edits to final token)
            this.enqueue(resultState);
            return resultState;
          }
        }
      }

      // Else:  either empty OR we've detected a 'new context'.  Initialize from scratch; no prior input information is
      // available.  Only the results of the prior inputs are known.
      let state = ContextTracker.modelContextState(tokenizedContext);
      state.context = context;
      // TODO:  assign SearchSpace (used for any edits to final token)
      this.enqueue(state);
      return state;
    }
  }
}