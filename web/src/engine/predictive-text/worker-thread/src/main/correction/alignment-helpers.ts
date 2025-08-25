/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-07-30
 *
 * This file defines methods used as helpers when aligning cached context state
 * information with incoming contexts and when validating partial substitution
 * edits for aligned context tokens.
 */

import { SENTINEL_CODE_UNIT } from '@keymanapp/models-templates';
import { ClassicalDistanceCalculation, EditOperation } from "./classical-calculation.js";

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
 * Determines the proper 'last match' index for a tokenized sequence based on its edit path.
 *
 * In particular, this method is designed to handle the following cases:
 * - ['to', ' ', 'apple', ' ', ''] => ['to', ' ', 'apply', ' ', '']
 * - ['to', ' ', 'apple', ' ', ''] => ['to', ' ', 'apply', ' ', 'n']
 *
 * Edit path for these example case:
 * - ['match', 'match', 'substitute', 'match', 'match']
 * - ['match', 'match', 'substitute', 'match', 'substitute']
 *
 * In cases such as these, the late whitespace match should be considered 'edited'. While the
 * ' ' is unedited, it follows the edited 'apple' => 'apply', so it must have been deleted and
 * then re-inserted.  As a result, the whitespace after 'to' is the true "last matched" token.
 *
 * Returns -1 if an unexpected edit other than 'substitute' occurs in the middle of the big
 * 'match' block.
 * @param editPath
 * @returns
 */
export function getEditPathLastMatch(editPath: EditOperation[]) {
  // Assertion:  for a long context, the bulk of the edit path should be a
  // continuous block of 'match' entries.  If there's anything but a substitution
  // in the middle, we have a context mismatch.
  //
  // That said, it is possible to apply a suggestion after a backspace.  Anything
  // after the substitution needs to be treated as a substitution rather than
  // a match.
  const firstMatch = editPath.indexOf('match');
  const lastMatch = editPath.lastIndexOf('match');
  if(firstMatch > -1) {
    for(let i = firstMatch+1; i <= lastMatch; i++) {
      if(editPath[i] != 'match') {
        return (editPath[i] == 'substitute') ? (i - 1) : -1;
      }
    }
  }

  return lastMatch;
}

/**
 * Aligns two tokens on a character-by-character basis as needed for higher, token-level alignment
 * operations.
 * @param incomingToken The incoming token value
 * @param matchingToken The pre-existing token value to use for comparison and alignment
 * @param forNearCaret  If `false`, disallows any substitutions and activates a leading-edge alignment
 * validation mode.
 * @returns
 */
export function isSubstitutionAlignable(
  incomingToken: string,
  matchingToken: string,
  forNearCaret?: boolean
): boolean {
  // 1 - Determine the edit path for the word.
  let subEditPath = ClassicalDistanceCalculation.computeDistance(
    [...matchingToken].map(value => ({key: value})),
    [...incomingToken].map(value => ({key: value})),
    // Use max length in case the word is actually already partly out of
    // the sliding context window.
    Math.max(incomingToken.length, matchingToken.length)
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
      // When this is called for a word not adjacent to the caret, its letters shouldn't be
      // substituted - that operation doesn't happen at a sliding context-window edge.
      return false;
    } else if(firstMatch > -1) {
      const lastMatch     = subEditPath.lastIndexOf('match');
      // Should not have inserts or deletes on both sides of matched text!
      // Due to how the edit path is calculated, an insert or delete could appear after the
      // firstMatch - especially in the case of adjacent double letters.
      //
      // Ex: applesauce => plesauce tends to say 'match', then 'delete', on the two 'p's.
      if(firstInsert > -1 && firstInsert < firstMatch && subEditPath.lastIndexOf('insert') > lastMatch) {
        return false;
      } else if(firstDelete > -1 && firstDelete < firstMatch && subEditPath.lastIndexOf('delete') > lastMatch) {
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

/**
 * Determines the alignment between a new, incoming tokenization source and the
 * tokenization modeled by the current instance.
 * @param tokenizationToMatch Raw strings corresponding to the tokenization of the original context
 * @param incomingTokenization Raw strings corresponding to the tokenization of the incoming context
 * @param isSliding Notes if the context window is full (and sliding-alignment is particularly needed)
 * @param noSubVerify When true, this disables inspection of 'substitute' transitions that avoids
 * wholesale replacement of the original token.
 * @returns Alignment data that details if and how the incoming tokenization aligns with
 * the tokenization modeled by this instance.
 */
export function computeAlignment(
  tokenizationToMatch: string[],
  incomingTokenization: string[],
  isSliding: boolean,
  noSubVerify?: boolean
): ContextStateAlignment {
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

  const lastMatch = getEditPathLastMatch(editPath);

  // Assertion:  for a long context, the bulk of the edit path should be a
  // continuous block of 'match' entries.  If there's anything else in
  // the middle, we have a context mismatch.
  if(lastMatch == -1) {
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