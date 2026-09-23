
/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2026-07-27
 *
 * This file contains tests designed to ensure predictive text does not
 * provide matching 'keep' and 'revert' suggestions in any context.
 */


import { assert } from 'chai';

import { LexicalModelTypes } from '@keymanapp/common-types';
import { prependReversion, type TransitionReversionView } from "@keymanapp/lm-worker/test-index";

import Suggestion = LexicalModelTypes.Suggestion;

describe('prependReversion', () => {
  it(`prepends reversions when reverting a non-'keep' suggestion`, () => {
    // context: Original was appl+u, corrected to apply.  Reached via bksp.
    const suggestions: Suggestion[] = [{
      tag: 'keep',
      transform: { insert: 'apply', deleteLeft: 6, id: 3 },
      displayAs: '"apply"',
      id: 5,
      matchesModel: false
    } as Suggestion];

    const revertable: TransitionReversionView = {
      reversion: {
        tag: 'revert',
        transform: { insert: 'u', deleteLeft: 0, id: 3 },
        id: -3,
        displayAs: '"applu"'
      },
      final: {
        suggestions: [{
          tag: 'keep',
          transform: { insert: 'applu', deleteLeft: 4, id: 3 },
          displayAs: '"applu"',
          id: 2,
          matchesModel: false
        } as Suggestion, {
          transform: { insert: 'apply', deleteLeft: 4, id: 3 },
          displayAs: 'apply',
          id: 3
        }, {
          transform: { insert: 'applied', deleteLeft: 4, id: 3 },
          displayAs: 'applied',
          id: 4
        }]
      }
    };

    prependReversion(suggestions, revertable);

    assert.includeMembers(suggestions, [revertable.reversion]);
  });

  it(`does not prepend reversions when reverting a 'keep' suggestion`, () => {
    // context: Original was appl+u, corrected to apply
    const suggestions: Suggestion[] = [{
      tag: 'keep',
      transform: { insert: 'applu', deleteLeft: 5, id: 3 },
      displayAs: '"applu"',
      id: 5,
      matchesModel: false
    } as Suggestion];

    const revertable: TransitionReversionView = {
      reversion: {
        tag: 'revert',
        transform: { insert: 'u', deleteLeft: 0, id: 3 },
        id: -2,
        displayAs: '"applu"'
      },
      final: {
        suggestions: [{
          tag: 'keep',
          transform: { insert: 'applu', deleteLeft: 4, id: 3 },
          displayAs: '"applu"',
          id: 2,
          matchesModel: false
        } as Suggestion, {
          transform: { insert: 'apply', deleteLeft: 4, id: 3 },
          displayAs: 'apply',
          id: 3
        }, {
          transform: { insert: 'applied', deleteLeft: 4, id: 3 },
          displayAs: 'applied',
          id: 4
        }]
      }
    };

    prependReversion(suggestions, revertable);

    assert.notIncludeMembers(suggestions, [revertable.reversion]);
  });
});