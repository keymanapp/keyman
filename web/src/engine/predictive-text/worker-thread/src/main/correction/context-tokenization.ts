/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-07-30
 *
 * Represents cached data about one potential tokenization of contents of
 * the sliding context window for one specific instance of context state.
 */

import { ContextToken } from './context-token.js';
import { ClassicalDistanceCalculation, EditOperation } from './classical-calculation.js';
import { getEditPathLastMatch, isSubstitutionAlignable } from './alignment-helpers.js';
import { SENTINEL_CODE_UNIT, Token } from '@keymanapp/models-templates';

import { LexicalModelTypes } from '@keymanapp/common-types';
import Distribution = LexicalModelTypes.Distribution;
import LexicalModel = LexicalModelTypes.LexicalModel;
import Transform = LexicalModelTypes.Transform;
import TransformUtils from '../transformUtils.js';

/**
 * Represents token-count values resulting from an alignment attempt between two
 * different modeled context states.
 */
export type ContextStateAlignment = {
  /**
   * Denotes whether or not alignment is possible between two contexts.
   */
  canAlign: false,

  /**
   * Indicates the edit path that could not be handled.  (Useful for error reporting)
   *
   * The edit path does not include actual user text and is sanitized.
   */
  editPath: EditOperation[];
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

/**
 * This class represents the sequence of tokens (words and whitespace blocks)
 * held within the active sliding context-window at a single point in time.
 */
export class ContextTokenization {
  readonly tokens: ContextToken[];
  readonly alignment?: ContextStateAlignment;

  constructor(priorToClone: ContextTokenization);
  constructor(tokens: ContextToken[], alignment?: ContextStateAlignment);
  constructor(param1: ContextToken[] | ContextTokenization, alignment?: ContextStateAlignment) {
    if(!(param1 instanceof ContextTokenization)) {
      const tokens = param1;
      this.tokens = [].concat(tokens);
      this.alignment = alignment;
    } else {
      const priorToClone = param1;
      this.tokens = priorToClone.tokens.map((entry) => new ContextToken(entry));
      this.alignment = {...priorToClone.alignment};
    }
  }

  /**
   * Returns the token adjacent to the text insertion point.
   */
  get tail(): ContextToken {
    return this.tokens[this.tokens.length - 1];
  }

  /**
   * Returns a plain-text string representing the most probable representation for all
   * tokens represented by this tokenization instance.
   */
  get exampleInput(): string[] {
    return this.tokens
      // Hide any tokens representing invisible wordbreaks.  (Thinking ahead to phrase-level possibilities)
      .filter(token => token.exampleInput !== null)
      .map(token => token.exampleInput);
  }

  /**
   * Determines the alignment between a new, incoming tokenization source and the
   * tokenization modeled by the current instance.
   * @param incomingTokenization Raw strings corresponding to the tokenization of the incoming context
   * @param isSliding Notes if the context window is full (and sliding-alignment is particularly needed)
   * @param noSubVerify When true, this disables inspection of 'substitute' transitions that avoids
   * wholesale replacement of the original token.
   * @returns Alignment data that details if and how the incoming tokenization aligns with
   * the tokenization modeled by this instance.
   */
  computeAlignment(incomingTokenization: string[], isSliding: boolean, noSubVerify?: boolean): ContextStateAlignment {
    // Map the tokenized state to an edit-distance friendly version.
    const tokenizationToMatch = this.exampleInput;

    const src = tokenizationToMatch.map(value => ({key: value}));
    const dst = incomingTokenization.map(value => ({key: value}));

    // let changedEmptyTail = false;
    if(dst[dst.length - 1].key == '') {
      // Only allow matching if the tokenizations are identical, thus the empty
      // token was unaffected.
      if(src.length != dst.length || src[dst.length - 1].key != '') {
        // Do not allow empty-token matches to match each other; this complicates
        // things when applying zero-root suggestions.
        //
        // The SENTINEL char should never appear in raw text, thus should never
        // match anything in the "tokenization to match".
        dst[dst.length - 1].key = SENTINEL_CODE_UNIT;
      }
    }

    // Inverted order, since 'match' existed before our new context.
    let mapping = ClassicalDistanceCalculation.computeDistance(
      src,
      dst,
      // Diagonal width to consider must be at least 2, as adding a single
      // whitespace after a token tends to add two tokens: one for whitespace,
      // one for the empty token to follow it.
      3
    );

    let editPath = mapping.editPath();
    const failure: ContextStateAlignment = {
      canAlign: false,
      editPath
    };

    // Special case:  new context bootstrapping - first token often substitutes.
    // The text length is small enough that no words should be able to rotate out the start of the context.
    // Special handling needed in case of no 'match'; the rest of the method assumes at least one 'match'.
    if(editPath.length <= 3 && (editPath[0] == 'substitute' || editPath[0] == 'match')) {
      let matchCount = 0;
      let subCount = 0;
      for(let i = 0; i < editPath.length; i++) {
        if(editPath[i] == 'substitute') {
          subCount++;
          if(!noSubVerify && !isSubstitutionAlignable(incomingTokenization[i], tokenizationToMatch[i], true)) {
            return failure;
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

    if(firstMatch == -1) {
      // If there are no matches, there's no alignment.
      return failure;
    }

    // Transpositions are not allowed at the token level during context alignment.
    if(editPath.find((entry) => entry.indexOf('transpose') > -1)) {
      return failure;
    }

    const lastMatch = getEditPathLastMatch(editPath);

    // Assertion:  for a long context, the bulk of the edit path should be a
    // continuous block of 'match' entries.  If there's anything else in
    // the middle, we have a context mismatch.
    if(lastMatch == -1) {
      return failure;
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
      return failure;
    }
    const tailSubstituteLength = (editPath.length - 1 - lastMatch) - tailInsertLength - tailDeleteLength;

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
            failure;
          }
          leadTokensRemoved++;
          break;
        case 'substitute':
          // We only allow for one leading token to be substituted.
          //
          // Any extras in the front would be pure inserts, not substitutions, due to
          // the sliding context window and its implications.
          if(leadSubstitutions++ > 0) {
            failure;
          }

          // Find the word before and after substitution.
          const incomingIndex = i - (leadTokensRemoved > 0 ? leadTokensRemoved : 0);
          const matchingIndex = i + (leadTokensRemoved < 0 ? leadTokensRemoved : 0);
          const incomingSub = incomingTokenization[incomingIndex];
          const matchingSub = tokenizationToMatch[matchingIndex];

          const atSlidePoint = isSliding && (incomingIndex == 0 || matchingIndex == 0);

          // Double-check the word - does the 'substituted' word itself align?
          //
          // Exception: if the word is at the start of the context window and the
          // context window is likely sliding, don't check it.
          if(!noSubVerify && !atSlidePoint && !isSubstitutionAlignable(incomingSub, matchingSub)) {
            failure;
          }

          // There's no major need to drop parts of a token being 'slid' out of the context window.
          // We'll leave it intact and treat it as a 'match'
          matchLength++;
          break;
        case 'insert':
          // Only allow an insert at the leading edge, as with 'delete's.
          if(priorEdit && priorEdit != 'insert') {
            failure;
          }
          // In case of backspaces, it's also possible to 'insert' a 'new'
          // token - an old one that's slid back into view.
          leadTokensRemoved--;
          break;
        default:
          // No 'match' can exist before the first found index for a 'match'.
          // No 'transpose-' edits should exist within this section, either.
          failure;
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

  /**
   * Given an alignment between an incoming tokenization context and the current tokenization
   * instance, this method will produce a new ContextTokenization instance for the incoming context
   * that reuses as many correction-search intermediate results as possible.
   * @param tokenizedContext  A single tokenization for the incoming context that aligns well
   * with the tokenization represented by the current instance.
   * @param alignment The alignment, as determined by a prior call to `computeAlignment`.
   * @param lexicalModel The active lexical model
   * @param alignedTransformDistribution The tokenized version of the input distribution accounting
   * for the difference between the context represented by this instance and that of `tokenizedContext`.
   * @returns
   */
  transitionTo(
    tokenizedContext: Token[],
    alignment: ContextStateAlignment,
    lexicalModel: LexicalModel,
    // FUTURE NOTE:  especially for epic-dict-breaker, we'll want an array of these - to align across multiple transitions
    // in case word boundaries shift back and forth.
    alignedTransformDistribution: Distribution<Transform[]>
  ): ContextTokenization {
    if(!alignment.canAlign) {
      return null;
    }

    const {
      leadTokenShift,
      matchLength,
      tailEditLength,
      tailTokenShift
    } = alignment;
    const hasDistribution = alignedTransformDistribution?.length > 0;

    // If we have a perfect match with a pre-existing tokenization, no mutations have
    // happened; just re-use the old context tokenization.
    if(tailEditLength == 0 && leadTokenShift == 0 && tailTokenShift == 0) {
      // We must build a new instance in case the original did not have
      // alignment data (like when it's the initial context!)
      return new ContextTokenization(this.tokens, alignment);
    } else {
      // If we didn't get any input, we really should perfectly match
      // a previous context state.  If such a state is out of our cache,
      // it should simply be rebuilt.
      if(!hasDistribution) {
        return null;
      }
    }

    // If mutations HAVE happened, we have work to do.
    const tokenization = this.tokens.map((token) => new ContextToken(token));

    if(leadTokenShift < 0) {
      tokenization.splice(0, -leadTokenShift);
    } else if(leadTokenShift > 0) {
      // TODO:  insert token(s) at the start to match the text that's back within the
      // sliding context window.
      //
      // (was not part of original source method [`attemptContextMatch`])
      return null;
    }

    // If no TAIL mutations have happened, we're safe to return now.
    if(tailEditLength == 0 && tailTokenShift == 0) {
      return new ContextTokenization(tokenization, alignment);
    }

    // ***

    const incomingOffset = (leadTokenShift > 0 ? leadTokenShift : 0);
    const matchingOffset = (leadTokenShift < 0 ? -leadTokenShift : 0);

    // If a word is being slid out of context-window range, start trimming it - we should
    // no longer need to worry about reusing its original correction-search results.
    if(matchLength > 0 && this.tokens[matchingOffset].exampleInput != tokenizedContext[incomingOffset].text) {
      //this.tokens[matchingOffset]'s clone is at tokenization[0] after the splice call in a previous block.
      tokenization[0] = new ContextToken(lexicalModel, tokenizedContext[incomingOffset].text);
    }

    // first non-matched tail index within the incoming context
    const incomingTailUpdateIndex = matchLength + incomingOffset;
    // first non-matched tail index in `matchState`, the base context state.
    const matchingTailUpdateIndex = matchLength + matchingOffset;

    // The assumed input from the input distribution is always at index 0.
    const tokenizedPrimaryInput = hasDistribution ? alignedTransformDistribution[0].sample : null;
    // first index:  original sample's tokenization
    // second index:  token index within original sample
    const tokenDistribution = alignedTransformDistribution.map((entry) => {
      return entry.sample.map((sample) => {
        return {
          sample: sample,
          p: entry.p
        }
      });
    });

    // Using these as base indices...
    let tailIndex = 0;
    for(let i = 0; i < tailEditLength; i++) {
      // do tail edits
      const incomingIndex = i + incomingTailUpdateIndex;
      const matchingIndex = i + matchingTailUpdateIndex;

      const incomingToken = tokenizedContext[incomingIndex];
      const matchedToken = this.tokens[matchingIndex];

      let primaryInput = hasDistribution ? tokenizedPrimaryInput[i] : null;
      const isBackspace = primaryInput && TransformUtils.isBackspace(primaryInput);
      let token: ContextToken;

      if(isBackspace) {
        token = new ContextToken(lexicalModel, incomingToken.text);
        token.searchSpace.inputSequence.forEach((entry) => entry[0].sample.id = primaryInput.id);
      } else {
        // Assumption:  there have been no intervening keystrokes since the last well-aligned context.
        // (May not be valid with epic/dict-breaker or with complex, word-boundary crossing transforms)
        token = new ContextToken(matchedToken);
        // Erase any applied-suggestion transition ID; it is no longer valid.
        token.appliedTransitionId = undefined;
        token.searchSpace.addInput(tokenDistribution.map((seq) => seq[tailIndex] ?? { sample: { insert: '', deleteLeft: 0 }, p: 1 }));
      }

      tokenization[incomingIndex] = token;
      tailIndex++;
    }

    if(tailTokenShift < 0) {
      // delete tail tokens
      for(let i = 0; i > tailTokenShift; i--) {
        // If ALL that remains are deletes, we're good to go.
        //
        // This may not be the token at the index, but since all that remains are deletes,
        // we'll have deleted the correct total number from the end once all iterations
        // are done.
        tokenization.pop();
      }
    } else {
      for(let i = tailEditLength; i < tailEditLength + tailTokenShift; i++) {
        // create tail tokens
        const incomingIndex = i + incomingTailUpdateIndex;
        const incomingToken = tokenizedContext[incomingIndex];
        // // Assertion:  there should be no matching token; this should be a newly-appended token.
        // const matchingIndex = i + tailEditLength + matchingTailUpdateIndex;

        const primaryInput = hasDistribution ? tokenizedPrimaryInput[i] : null;
        let pushedToken = new ContextToken(lexicalModel);

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
        tokenization.push(pushedToken);
        tailIndex++;
      }
    }

    return new ContextTokenization(tokenization, alignment);
  }
}