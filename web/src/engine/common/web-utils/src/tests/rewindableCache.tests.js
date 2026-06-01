/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-08-01
 *
 * This test suite provides basic unit tests for the functionality of
 * the RewindableCache class, which tracks the original ordering of
 * cached entries, forgets the oldest when too full, and also allows
 * removal of all entries _more recent_ than a specified key's entry.
 */

import { assert } from 'chai';
import { RewindableCache } from '@keymanapp/web-utils';

describe("RewindableCache", () => {
  it("stores all entries while capacity remains", () => {
    const cache = new RewindableCache(5);
    cache.add(1, 'a');
    cache.add(2, 'b');
    cache.add(3, 'c');
    cache.add(4, 'd');
    cache.add(5, 'e');

    assert.sameMembers([...cache.keys()], [1, 2, 3, 4, 5]);
    assert.sameMembers([...cache.keys().map((k) => cache.get(k))], ['a', 'b', 'c', 'd', 'e']);
    assert.equal(cache.size, 5);
  });

  it("clears all entries when requested", () => {
    const cache = new RewindableCache(5);
    cache.add(1, 'a');
    cache.add(2, 'b');
    cache.add(3, 'c');
    cache.add(4, 'd');
    cache.add(5, 'e');

    cache.clear();

    assert.sameMembers([...cache.keys()], []);
    assert.sameMembers([...cache.keys().map((k) => cache.get(k))], []);
    assert.equal(cache.size, 0);
  });

  it("keys() iterates from most to least recent (no manipulation)", () => {
    const cache = new RewindableCache(5);
    cache.add(1, 'a');
    cache.add(2, 'b');
    cache.add(3, 'c');
    cache.add(4, 'd');
    cache.add(5, 'e');

    assert.sameOrderedMembers([...cache.keys()], [5, 4, 3, 2, 1]);
    assert.sameOrderedMembers([...cache.keys().map((k) => cache.get(k))], ['e', 'd', 'c', 'b', 'a']);
    assert.equal(cache.size, 5);
  });

  it("forgets the oldest entry when no capacity remains", () => {
    const cache = new RewindableCache(2);
    cache.add(1, 'a');
    cache.add(2, 'b');
    cache.add(3, 'c');
    cache.add(4, 'd');
    cache.add(5, 'e');

    assert.sameOrderedMembers([...cache.keys()], [5, 4]);
    assert.sameOrderedMembers([...cache.keys().map((k) => cache.get(k))], ['e', 'd']);
    assert.equal(cache.size, 2);
  });

  it("forgets newer entries than provided key", () => {
    const cache = new RewindableCache(5);
    cache.add(1, 'a');
    cache.add(2, 'b');
    cache.add(3, 'c');
    cache.add(4, 'd');
    cache.add(5, 'e');

    // make it more recently-accessed, but don't remove it.
    cache.rewindTo(3);

    cache.add(2, 'b'); // 2 is still ORIGINALLY older than 3, even if accessed more recently.
    assert.sameOrderedMembers([...cache.keys()], [2, 3, 1]);
    assert.sameOrderedMembers([...cache.keys().map((k) => cache.get(k))], ['b', 'c', 'a']);
    assert.equal(cache.size, 3);
  });

  it("forgets no entries if invalid key is provided", () => {
    const cache = new RewindableCache(2);
    cache.add(1, 'a');
    cache.add(2, 'b');
    cache.add(3, 'c'); // Entry 1 no longer valid
    cache.add(4, 'd'); //       2
    cache.add(5, 'e'); //       3

    assert.throws(() => cache.rewindTo(3));
  });

  it("does not throw errors for keys no longer in the cache", () => {
    const cache = new RewindableCache(2);
    cache.add(1, 'a');
    cache.add(2, 'b');
    cache.add(3, 'c'); // Entry 1 no longer valid
    cache.add(4, 'd'); //       2
    cache.add(5, 'e'); //       3

    assert.isUndefined(cache.get(2), 'outdated cache entry should be forgotten');
  });
});