/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-07-30
 *
 * This file defines methods used as helpers when aligning cached context state
 * information with incoming contexts and when validating partial substitution
 * edits for aligned context tokens.
 */

import { ClassicalDistanceCalculation, EditOperation } from "./classical-calculation.js";

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