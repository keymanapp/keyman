/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-07-30
 *
 * Represents cached data about one potential tokenization of contents of
 * the sliding context window for one specific instance of context state.
 */

import { ContextToken } from './context-token.js';
import { ClassicalDistanceCalculation } from './classical-calculation.js';
import { getEditPathLastMatch, isSubstitutionAlignable } from './alignment-helpers.js';

/**
 * Represents token-count values resulting from an alignment attempt between two
 * different modeled context states.
 */
export type ContextStateAlignment = {
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
   * @param noSubVerify When true, this disables inspection of 'substitute' transitions that avoids
   * wholesale replacement of the original token.
   * @returns Alignment data that details if and how the incoming tokenization aligns with
   * the tokenization modeled by this instance.
   */
  computeAlignment(incomingTokenization: string[], noSubVerify?: boolean): ContextStateAlignment {
    // Map the tokenized state to an edit-distance friendly version.
    const tokenizationToMatch = this.exampleInput;

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
          if(!noSubVerify && !isSubstitutionAlignable(incomingTokenization[i], tokenizationToMatch[i], true)) {
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
          if(!noSubVerify && !isSubstitutionAlignable(incomingSub, matchingSub)) {
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
}