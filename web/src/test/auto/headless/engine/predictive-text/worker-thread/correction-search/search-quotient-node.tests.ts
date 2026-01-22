import { assert } from 'chai';

import { jsonFixture } from '@keymanapp/common-test-resources/model-helpers.mjs';
import { LegacyQuotientRoot, models, quotientPathHasInputs } from '@keymanapp/lm-worker/test-index';

import { buildSimplePathSplitFixture } from './search-quotient-spur.tests.js';

import TrieModel = models.TrieModel;

const testModel = new TrieModel(jsonFixture('models/tries/english-1000'));

describe('quotientNodeHasParents()', () => {
  it('matches an empty array on root SearchPaths', () => {
    assert.isTrue(quotientPathHasInputs(new LegacyQuotientRoot(testModel), []));
  });

  it('matches all path inputs when provided in proper order', () => {
    const { paths, distributions } = buildSimplePathSplitFixture();
    assert.isTrue(quotientPathHasInputs(paths[4], distributions));
  });

  it('does not match when any path input component is missing', () => {
    const { paths, distributions } = buildSimplePathSplitFixture();
    assert.isFalse(quotientPathHasInputs(paths[4], distributions.slice(1)));
    assert.isFalse(quotientPathHasInputs(paths[4], distributions.slice(2)));
    assert.isFalse(quotientPathHasInputs(paths[4], distributions.slice(3)));
    assert.isFalse(quotientPathHasInputs(paths[4], distributions.slice(0, 3)));
    assert.isFalse(quotientPathHasInputs(paths[4], distributions.slice(0, 1).concat(distributions.slice(2))));
  });

  it('does not match when path inputs are not in proper order', () => {
    const { paths, distributions } = buildSimplePathSplitFixture();
    assert.isFalse(quotientPathHasInputs(paths[4], distributions.slice().reverse()));

    // Random shuffle.
    let shuffled: typeof distributions;
    let isShuffled: boolean;
    do {
      shuffled = distributions.slice().sort(() => Math.random() * 2 - 1);
      // Validate that we actually shuffled - that we didn't land on the original order!
      isShuffled = false;
      for(let i = 0; i < distributions.length; i++) {
        if(distributions[i] != shuffled[i]) {
          isShuffled = true;
          break;
        }
      }
    } while(!isShuffled);
    assert.isFalse(quotientPathHasInputs(paths[4], shuffled));
  });
});

