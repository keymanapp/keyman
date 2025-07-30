import { applyTransform, buildMergedTransform, Token } from '@keymanapp/models-templates';
import { KMWString } from '@keymanapp/web-utils';

import { ClassicalDistanceCalculation, EditOperation } from './classical-calculation.js';
import TransformUtils from '../transformUtils.js';
import { determineModelTokenizer } from '../model-helpers.js';
import { tokenizeTransform, tokenizeTransformDistribution } from './transform-tokenization.js';
import { LexicalModelTypes } from '@keymanapp/common-types';
import Context = LexicalModelTypes.Context;
import Distribution = LexicalModelTypes.Distribution;
import LexicalModel = LexicalModelTypes.LexicalModel;
import Suggestion = LexicalModelTypes.Suggestion;
import Transform = LexicalModelTypes.Transform;
import { ContextToken } from './context-token.js';

export function textToCharTransforms(text: string, transformId?: number) {
  let perCharTransforms: Transform[] = [];

  for(let i=0; i < KMWString.length(text); i++) {
    let char = KMWString.charAt(text, i); // is SMP-aware

    let transform: Transform = {
      insert: char,
      deleteLeft: 0
    };

    if(transformId) {
      transform.id = transformId
    }

    perCharTransforms.push(transform);
  }

  return perCharTransforms;
}

/**
 * Determines the proper 'last match' index for a tokenized sequence based on its edit path.
 *
 * In particular, this method is designed to handle the following case:
 * ['to', 'apple', ' ', ''] => ['to', 'apply', ' ', 'n']
 *
 * Edit path for this example case:
 * ['match', 'substitute', 'match', 'substitute']
 *
 * In cases such as these, the whitespace match should be considered 'edited'. While the ' '
 * is unedited, it follows the edited 'apple' => 'apply', so it must have been deleted and
 * then re-inserted.  As a result, 'to' is the true "last matched" token.
 * @param editPath
 * @returns
 */
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

export class TrackedContextState {
  // Stores the post-transform Context.  Useful as a debugging reference, but also used to
  // pre-validate context state matches in case of discarded changes from multitaps.
  taggedContext: Context;
  model: LexicalModel;

  tokens: ContextToken[];
  /**
   * How many tokens were removed from the start of the best-matching ancestor.
   * Useful for restoring older states, e.g., when the user moves the caret backwards, we can recover the context at that position.
   */
  indexOffset: number;

  constructor(source: TrackedContextState);
  constructor(model: LexicalModel);
  constructor(obj: TrackedContextState | LexicalModel) {
    if(obj instanceof TrackedContextState) {
      let source = obj;
      // Be sure to deep-copy the tokens!  Pointer-aliasing is bad here.
      this.tokens = source.tokens.map((token) => new ContextToken(token));

      this.indexOffset = 0;
      this.model = obj.model;
      this.taggedContext = obj.taggedContext;
    } else {
      let lexicalModel = obj;
      this.tokens = [];
      this.indexOffset = Number.MIN_SAFE_INTEGER;
      this.model = lexicalModel;
    }
  }

  get head(): ContextToken {
    return this.tokens[0];
  }

  get tail(): ContextToken {
    return this.tokens[this.tokens.length - 1];
  }

  set tail(token: ContextToken) {
    this.tokens[this.tokens.length - 1] = token;
  }

  popHead() {
    this.tokens.splice(0, 1);
    this.indexOffset -= 1;
  }

  pushTail(token: ContextToken) {
    this.tokens.push(token);
  }

  toRawTokenization() {
    let sequence: string[] = [];

    for(let token of this.tokens) {
      // Hide any tokens representing wordbreaks.  (Thinking ahead to phrase-level possibilities)
      if(token.exampleInput !== null) {
        sequence.push(token.exampleInput);
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

/**
 * Represents token-count values resulting from an alignment attempt between two
 * different modeled context states.
 */
type TrackedContextStateAlignment = {
  /**
   * Denotes whether or not alignment is possible between two contexts.
   */
  canAlign: false
} | {
  /**
   * Denotes whether or not alignment is possible between two contexts.
   */
  canAlign: true,
  /**
   * Notes the number of tokens added to the head of the 'incoming'/'new' context
   * of the contexts being aligned.  If negative, the incoming context deleted
   * a token found in the 'original' / base context.
   *
   * For the alignment, [base context index] + leadTokenShift = [incoming context index].
   */
  leadTokenShift: number,
  /**
   * The count of tokens perfectly aligned, with no need for edits, for two successfully-
   * alignable contexts.
   */
  matchLength: number,
  /**
   * The count of tokens at the tail perfectly aligned (existing in both contexts) but
   * edited for two successfully-alignable contexts.  These tokens directly follow those
   * that need no edits.
   */
  tailEditLength: number,
  /**
   * The count of new tokens added at the end of the incoming context for two aligned contexts.
   * If negative, the incoming context deleted a previously-existing token from the original.
   */
  tailTokenShift: number
};

export class ContextTracker extends CircularArray<TrackedContextState> {
  /**
   * Aligns two tokens on a character-by-character basis as needed for higher, token-level alignment
   * operations.
   * @param incomingToken The incoming token value
   * @param matchingToken The pre-existing token value to use for comparison and alignment
   * @param forNearCaret  If `false`, disallows any substitutions and activates a leading-edge alignment
   * validation mode.
   * @returns
   */
  static isSubstitutionAlignable(
    incomingToken: string,
    matchingToken: string,
    forNearCaret?: boolean
  ): boolean {
    // 1 - Determine the edit path for the word.
    let subEditPath = ClassicalDistanceCalculation.computeDistance(
      [...matchingToken].map(value => ({key: value})),
      [...incomingToken].map(value => ({key: value})),
      // Diagonal width to consider must be at least 2, as adding a single
      // whitespace after a token tends to add two tokens: one for whitespace,
      // one for the empty token to follow it.
      3
    ).editPath();

    const firstInsert = subEditPath.indexOf('insert');
    const firstDelete = subEditPath.indexOf('delete');

    // 2 - deletions and insertions should be mutually exclusive.
    // A fixed, unedited word can't slide across both 'left' and 'right' boundaries at the same time.
    if(firstInsert != -1 && firstDelete != -1) {
      return false;
    };

    // 3 - checks exclusive to leading-edge conditions
    if(!forNearCaret) {
      const firstSubstitute = subEditPath.indexOf('substitute');
      const firstMatch      = subEditPath.indexOf('match');
      if(firstSubstitute > -1) {
        return false;
      } else if(firstMatch > -1) {
        // Should not have inserts on both sides of matched text!
        if(firstInsert > -1 && firstInsert < firstMatch && subEditPath.lastIndexOf('insert') > firstMatch) {
          return false;
        } else if(firstDelete > -1 && firstDelete < firstMatch && subEditPath.lastIndexOf('delete') > firstMatch) {
          return false;
        }
      }

      // Further checks below are oriented for text/tokens at the caret.
      return true;
    }

    // 4 - check the stats for total edits of each type and validate that edits don't overly exceed
    // original characters.
    const editCount = {
      matchMove: 0,
      rawEdit: 0
    };

    subEditPath.forEach((entry) => {
      switch(entry) {
        case 'transpose-end':
        case 'transpose-start':
        case 'match':
          editCount.matchMove++;
          break;
        case 'insert':
        case 'transpose-insert':
        case 'delete':
        case 'transpose-delete':
        case 'substitute':
          editCount.rawEdit++;
      }
    });

    // We shouldn't have more raw substitutions, inserts, and deletes than matches + transposes,
    // though allowing +1 as a fudge factor.
    // The 'a' => 'à' pattern can be a reasonably common Keyman keyboard rule and
    // is one substitution, zero matches in NFC.
    if(editCount.matchMove + 1 < editCount.rawEdit) {
      return false;
    }

    return true;
  }

  static attemptTokenizedAlignment(
    incomingTokenization: string[],
    tokenizationToMatch: string[]
  ): TrackedContextStateAlignment {

    // Inverted order, since 'match' existed before our new context.
    let mapping = ClassicalDistanceCalculation.computeDistance(
      tokenizationToMatch.map(value => ({key: value})),
      incomingTokenization.map(value => ({key: value})),
      // Diagonal width to consider must be at least 2, as adding a single
      // whitespace after a token tends to add two tokens: one for whitespace,
      // one for the empty token to follow it.
      3
    );

    let editPath = mapping.editPath();
    // Special case:  new context bootstrapping - first token often substitutes.
    // The text length is small enough that no words should be able to rotate out the start of the context.
    // Special handling needed in case of no 'match'; the rest of the method assumes at least one 'match'.
    if(editPath.length <= 3 && (editPath[0] == 'substitute' || editPath[0] == 'match')) {
      let matchCount = 0;
      let subCount = 0;
      for(let i = 0; i < editPath.length; i++) {
        if(editPath[i] == 'substitute') {
          subCount++;
          if(!this.isSubstitutionAlignable(incomingTokenization[i], tokenizationToMatch[i], true)) {
            return {
              canAlign: false
            };
          }
        } else if(editPath[i] == 'match') {
          // If a substitution is already recorded, treat the 'match' as a substitution.
          if(subCount > 0) {
            subCount++;
          } else {
            matchCount++;
          }
        }
      }

      const insertCount = editPath.filter((entry) => entry == 'insert').length;
      const deleteCount = editPath.filter((entry) => entry == 'delete').length;

      return {
        canAlign: true,
        matchLength: matchCount,
        leadTokenShift: 0,
        tailEditLength: subCount,
        tailTokenShift: insertCount - deleteCount
      }
    }

    // From here on assumes that at least one 'match' exists on the path.
    // It all works great... once the context is long enough for at least one stable token.
    const firstMatch = editPath.indexOf('match');
    const lastMatch = getEditPathLastMatch(editPath);
    if(firstMatch == -1) {
      // If there are no matches, there's no alignment.
      return {
        canAlign: false
      };
    }

    // Transpositions are not allowed at the token level during context alignment.
    if(editPath.find((entry) => entry.indexOf('transpose') > -1)) {
      return {
        canAlign: false
      };
    }

    let matchLength = lastMatch - firstMatch + 1;
    let tailInsertLength = 0;
    let tailDeleteLength = 0;
    for(let i = lastMatch; i < editPath.length; i++) {
      if(editPath[i] == 'insert') {
        tailInsertLength++;
      } else if(editPath[i] == 'delete') {
        tailDeleteLength++;
      }
    }
    if(tailInsertLength > 0 && tailDeleteLength > 0) {
      // Something's gone weird if this happens; that should appear as a substitution instead.
      // Otherwise, we have a VERY niche edit scenario.
      return {
        canAlign: false
      };
    }
    const tailSubstituteLength = (editPath.length - 1 - lastMatch) - tailInsertLength - tailDeleteLength;

    // Assertion:  for a long context, the bulk of the edit path should be a
    // continuous block of 'match' entries.  If there's anything else in
    // the middle, we have a context mismatch.
    if(firstMatch > -1) {
      for(let i = firstMatch+1; i < lastMatch; i++) {
        if(editPath[i] != 'match') {
          return {
            canAlign: false
          };
        }
      }
    }

    // If we have a perfect match with a pre-existing context, no mutations have
    // happened; we have a 100% perfect match.
    if(firstMatch == 0 && lastMatch == editPath.length - 1) {
      return {
        canAlign: true,
        leadTokenShift: 0,
        matchLength,
        tailEditLength: tailSubstituteLength,
        tailTokenShift: tailInsertLength - tailDeleteLength
      };
    }

    // The edit path calc tries to put substitutes first, before inserts.
    // We don't want that on the leading edge.
    const lastEarlyInsert = editPath.lastIndexOf('insert', firstMatch);
    const firstSubstitute = editPath.indexOf('substitute');
    if(firstSubstitute > -1 && firstSubstitute < firstMatch && firstSubstitute < lastEarlyInsert) {
      editPath[firstSubstitute] = 'insert';
      editPath[lastEarlyInsert] = 'substitute';
    }

    // If mutations HAVE happened, we need to double-check the context-state alignment.
    let priorEdit: typeof editPath[0];
    let leadTokensRemoved = 0;
    let leadSubstitutions = 0;

    // The `i` index below aligns based upon the index within the `tokenizationToMatch` sequence
    // and how it would have to be edited to align to the `incomingTokenization` sequence.
    for(let i = 0; i < firstMatch; i++) {
      switch(editPath[i]) {
        case 'delete':
          // All deletions should appear at the sliding window edge; if a deletion appears
          // after the edge, but before the first match, something's wrong.
          if(priorEdit && priorEdit != 'delete') {
            return {
              canAlign: false
            };
          }
          leadTokensRemoved++;
          break;
        case 'substitute':
          // We only allow for one leading token to be substituted.
          //
          // Any extras in the front would be pure inserts, not substitutions, due to
          // the sliding context window and its implications.
          if(leadSubstitutions++ > 0) {
            return {
              canAlign: false
            };
          }

          // Find the word before and after substitution.
          const incomingSub = incomingTokenization[i - (leadTokensRemoved > 0 ? leadTokensRemoved : 0)];
          const matchingSub = tokenizationToMatch[i + (leadTokensRemoved < 0 ? leadTokensRemoved : 0)];

          // Double-check the word - does the 'substituted' word itself align?
          if(!this.isSubstitutionAlignable(incomingSub, matchingSub)) {
            return {
              canAlign: false
            };
          }

          // There's no major need to drop parts of a token being 'slid' out of the context window.
          // We'll leave it intact and treat it as a 'match'
          matchLength++;
          break;
        case 'insert':
          // Only allow an insert at the leading edge, as with 'delete's.
          if(priorEdit && priorEdit != 'insert') {
            return {
              canAlign: false
            };
          }
          // In case of backspaces, it's also possible to 'insert' a 'new'
          // token - an old one that's slid back into view.
          leadTokensRemoved--;
          break;
        default:
          // No 'match' can exist before the first found index for a 'match'.
          // No 'transpose-' edits should exist within this section, either.
          return {
            canAlign: false
          };
      }
      priorEdit = editPath[i];
    }

    // If we need some form of tail-token substitution verification, add that here.

    return {
      canAlign: true,
      // leadTokensRemoved represents the number of tokens that must be removed from the base context
      // when aligning the contexts.  Externally, it's more helpful to think in terms of the count added
      // to the incoming context.
      leadTokenShift: -leadTokensRemoved + 0, // add 0 in case of a 'negative zero', which affects unit tests.
      matchLength,
      tailEditLength: tailSubstituteLength,
      tailTokenShift: tailInsertLength - tailDeleteLength
    };
  }

  static attemptMatchContext(
    tokenizedContext: Token[],
    matchState: TrackedContextState,
    // the distribution should be tokenized already.
    transformSequenceDistribution?: Distribution<Transform[]>
  ): ContextMatchResult {
    // Map the previous tokenized state to an edit-distance friendly version.
    let matchContext: string[] = matchState.toRawTokenization();

    const alignmentResults = this.attemptTokenizedAlignment(tokenizedContext.map((token) => token.text), matchContext);

    if(!alignmentResults.canAlign) {
      return null;
    }

    const {
      leadTokenShift,
      matchLength,
      tailEditLength,
      tailTokenShift
    } = alignmentResults;

    const hasDistribution = transformSequenceDistribution && Array.isArray(transformSequenceDistribution);

    // If we have a perfect match with a pre-existing context, no mutations have
    // happened; just re-use the old context state.
    if(tailEditLength == 0 && leadTokenShift == 0 && tailTokenShift == 0) {
      return { state: matchState, baseState: matchState, headTokensRemoved: 0, tailTokensAdded: 0 };
    } else {
      // If we didn't get any input, we really should perfectly match
      // a previous context state.  If such a state is out of our cache,
      // it should simply be rebuilt.
      if(!hasDistribution) {
        return null;
      }
    }

    // If mutations HAVE happened, we have work to do.
    let state = matchState;

    if(leadTokenShift < 0) {
      state = new TrackedContextState(state);
      for(let i = 0; i > leadTokenShift; i--) {
        state.popHead();
      }
    } else if(leadTokenShift > 0) {
      // TODO:  insert token(s) at the start to match the text that's back within the
      // sliding context window.
      //
      // (was not part of original `attemptContextMatch`)
      return null;
    }

    // If no TAIL mutations have happened, we're safe to return now.
    if(tailEditLength == 0 && tailTokenShift == 0) {
      return {
        state: state,
        baseState: matchState,
        headTokensRemoved: -leadTokenShift,
        tailTokensAdded: tailTokenShift
      }
    }

    // ***

    // first non-matched tail index within the incoming context
    const incomingTailUpdateIndex = matchLength + (leadTokenShift > 0 ? leadTokenShift : 0);
    // first non-matched tail index in `matchState`, the base context state.
    const matchingTailUpdateIndex = matchLength - (leadTokenShift < 0 ? leadTokenShift : 0);

    // The assumed input from the input distribution is always at index 0.
    const tokenizedPrimaryInput = hasDistribution ? transformSequenceDistribution[0].sample : null;
    // first index:  original sample's tokenization
    // second index:  token index within original sample
    const tokenDistribution = transformSequenceDistribution.map((entry) => {
      return entry.sample.map((sample) => {
        return {
          sample: sample,
          p: entry.p
        }
      });
    });

    // // Gets distribution of token index 1s as excerpted from the sequences' distribution.
    // let a = tokenDistribution.map((sequence) => sequence[1]);

    // Using these as base indices...

    let tailIndex = 0;
    // let lastTailIndex = tailEditLength + (tailTokenShift > 0 ? tailTokenShift : 0);

    // Used to construct and represent the part of the incoming transform that
    // does not land as part of the final token in the resulting context.  This
    // component should be preserved by any suggestions that get applied.
    let preservationTransform: Transform;

    for(let i = 0; i < tailEditLength; i++) {
      // do tail edits
      const incomingIndex = i + incomingTailUpdateIndex;
      const matchingIndex = i + matchingTailUpdateIndex;

      const incomingToken = tokenizedContext[incomingIndex];
      const matchedToken = matchState.tokens[matchingIndex];

      let primaryInput = hasDistribution ? tokenizedPrimaryInput[i] : null;
      const isBackspace = primaryInput && TransformUtils.isBackspace(primaryInput);

      const isLastToken = incomingIndex == tokenizedContext.length - 1;

      if(isLastToken) {
        state = new TrackedContextState(state);
        // If this token's transform component is not part of the final token,
        // it's something we'll want to preserve even when applying suggestions
        // for the final token.
        //
        // Note:  will need a either a different approach or more specialized
        // handling if/when supporting phrase-level (multi-token) suggestions.
      } else {
        preservationTransform = preservationTransform && primaryInput ? buildMergedTransform(preservationTransform, primaryInput) : (primaryInput ?? preservationTransform);
      }
      let token: ContextToken;

      if(isBackspace) {
        token = new ContextToken(matchState.model, incomingToken.text);
        token.searchSpace.inputSequence.forEach((entry) => entry[0].sample.id = primaryInput.id);
      } else {
        token = new ContextToken(matchedToken);
        token.searchSpace.addInput(tokenDistribution.map((seq) => seq[tailIndex]));
      }

      state.tokens[incomingIndex] = token;
      tailIndex++;
    }

    if(tailTokenShift < 0) {
      if(state == matchState) {
        state = new TrackedContextState(state);
      }

      // delete tail tokens
      for(let i = 0; i > tailTokenShift; i--) {
        // If ALL that remains are deletes, we're good to go.
        //
        // This may not be the token at the index, but since all that remains are deletes,
        // we'll have deleted the correct total number from the end once all iterations
        // are done.
        state.tokens.pop();
      }
    } else {
      if(state == matchState) {
        state = new TrackedContextState(state);
      }

      for(let i = tailEditLength; i < tailEditLength + tailTokenShift; i++) {
        // create tail tokens
        const incomingIndex = i + incomingTailUpdateIndex;
        const incomingToken = tokenizedContext[incomingIndex];
        // // Assertion:  there should be no matching token; this should be a newly-appended token.
        // const matchingIndex = i + tailEditLength + matchingTailUpdateIndex;

        const primaryInput = hasDistribution ? tokenizedPrimaryInput[i] : null;

        if(!preservationTransform) {
          // Allows for consistent handling of "insert" cases; even if there's no edit
          // from a prior token, having a defined transform here indicates that
          // a new token has been produced.  This serves as a useful conditional flag
          // for prediction logic.
          preservationTransform = { insert: '', deleteLeft: 0 };
        }

        const isLastToken = incomingIndex == tokenizedContext.length - 1;
        if(!isLastToken) {
          preservationTransform = preservationTransform && primaryInput ? buildMergedTransform(preservationTransform, primaryInput) : (primaryInput ?? preservationTransform);
        }

        if(state == matchState) {
          state = new TrackedContextState(state);
        }

        let pushedToken = new ContextToken(state.model);

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
        let tokenDistribComponent = tokenDistribution.map((seq) => {
          const entry = seq[tailIndex];
          if(!entry || TransformUtils.isEmpty(entry.sample)) {
            return null;
          } else {
            return entry;
          }
        }).filter((entry) => !!entry);
        if(primaryInput) {
          let transformDistribution = tokenDistribComponent.length > 0 ? tokenDistribComponent : null;
          if(transformDistribution) {
            pushedToken.searchSpace.addInput(transformDistribution);
          }
        } else if(incomingToken.text) {
          // We have no transform data to match against an inserted token with text; abort!
          // Refer to #12494 for an example case; we currently can't map previously-committed
          // input transforms to a newly split-off token.
          return null;
        }
        pushedToken.isWhitespace = incomingToken.isWhitespace;

        // Auto-replaces the search space to correspond with the new token.
        state.pushTail(pushedToken);

        tailIndex++;
      }
    }

    return {
      state,
      baseState: matchState,
      preservationTransform,
      headTokensRemoved: alignmentResults.leadTokenShift < 0 ? -alignmentResults.leadTokenShift : 0,
      tailTokensAdded: alignmentResults.tailTokenShift
    };
  }

  private static modelContextState(
    tokenizedContext: Token[],
    lexicalModel: LexicalModel
  ): TrackedContextState {
    let baseTokens = tokenizedContext.map(function(entry) {
      let token = new ContextToken(lexicalModel, entry.text);

      if(entry.isWhitespace) {
        token.isWhitespace = true;
      }

      return token;
    });

    // And now build the final context state object, which includes whitespace 'tokens'.
    let state = new TrackedContextState(lexicalModel);

    while(baseTokens.length > 0) {
      state.pushTail(baseTokens.splice(0, 1)[0]);
    }

    if(state.tokens.length == 0) {
      let token = new ContextToken(lexicalModel);
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

    if(transformDistribution?.length == 0) {
      transformDistribution = null;
    }
    const inputTransform = transformDistribution?.[0];
    let transformTokenLength = 0;
    let tokenizedDistribution: Distribution<Transform[]> = null;
    if(inputTransform) {
      // These two methods apply transforms internally; do not mutate context here.
      // This particularly matters for the 'distribution' variant.

      // What if a pre-whitespace token has a final substitution as PART of an edit?
      // Say, ['apple', ' ', ''] => ['apply', ' ', 'n']
      // For now... we can't really handle that case well - modeling the 'e' => 'y' part.
      // Will likely require improvements to tokenizeTransform(), which doesn't yet handle
      // deleteLeft tokenization for transforms spanning tokens & whitespace.
      //
      // See: #14361.
      // There's a good shot attemptTokenizedAlignment would be useful for it.
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
