/*
 * Unit tests for the priority queue.
 */

import { assert } from 'chai';
import { IterableRecencyCache } from '@keymanapp/web-utils';

describe("IterableRecencyCache", () => {
  it("stores all entries while capacity remains", () => {
    const cache = new IterableRecencyCache(5);
    cache.add(1, 'a');
    cache.add(2, 'b');
    cache.add(3, 'c');
    cache.add(4, 'd');
    cache.add(5, 'e');

    assert.sameMembers([...cache.keys()], [1, 2, 3, 4, 5]);
    assert.sameMembers([...cache.keys().map((k) => cache.get(k))], ['a', 'b', 'c', 'd', 'e']);
    assert.equal(cache.count, 5);
  });

  it("clears all entries when requested", () => {
    const cache = new IterableRecencyCache(5);
    cache.add(1, 'a');
    cache.add(2, 'b');
    cache.add(3, 'c');
    cache.add(4, 'd');
    cache.add(5, 'e');

    cache.clear();

    assert.sameMembers([...cache.keys()], []);
    assert.sameMembers([...cache.keys().map((k) => cache.get(k))], []);
    assert.equal(cache.count, 0);
  });

  it("keys() iterates from most to least recent (no manipulation)", () => {
    const cache = new IterableRecencyCache(5);
    cache.add(1, 'a');
    cache.add(2, 'b');
    cache.add(3, 'c');
    cache.add(4, 'd');
    cache.add(5, 'e');

    assert.sameOrderedMembers([...cache.keys()], [5, 4, 3, 2, 1]);
    assert.sameOrderedMembers([...cache.keys().map((k) => cache.get(k))], ['e', 'd', 'c', 'b', 'a']);
    assert.equal(cache.count, 5);
  });

  it("forgets the oldest entry when no capacity remains", () => {
    const cache = new IterableRecencyCache(2);
    cache.add(1, 'a');
    cache.add(2, 'b');
    cache.add(3, 'c');
    cache.add(4, 'd');
    cache.add(5, 'e');

    assert.sameMembers([...cache.keys()], [4, 5]);
    assert.sameMembers([...cache.keys().map((k) => cache.get(k))], ['d', 'e']);

    assert.sameOrderedMembers([...cache.keys()], [5, 4]);
    assert.sameOrderedMembers([...cache.keys().map((k) => cache.get(k))], ['e', 'd']);
    assert.equal(cache.count, 2);
  });

  it("forgets newer entries than provided key", () => {
    const cache = new IterableRecencyCache(5);
    cache.add(1, 'a');
    cache.add(2, 'b');
    cache.add(3, 'c');
    cache.add(4, 'd');
    cache.add(5, 'e');

    cache.getAndRefresh(2);
    cache.clearNewerThanKey(3);

    assert.sameMembers([...cache.keys()], [1, 3]);
    assert.sameMembers([...cache.keys().map((k) => cache.get(k))], ['a', 'c']);

    assert.sameOrderedMembers([...cache.keys()], [3, 1]);
    assert.sameOrderedMembers([...cache.keys().map((k) => cache.get(k))], ['c', 'a']);
    assert.equal(cache.count, 2);
  });

  it("forgets no entries if invalid key is provided", () => {
    const cache = new IterableRecencyCache(2);
    cache.add(1, 'a');
    cache.add(2, 'b');
    cache.add(3, 'c');
    cache.add(4, 'd');
    cache.add(5, 'e');

    assert.doesNotThrow(() => cache.getAndRefresh(2));
    assert.throws(() => cache.clearNewerThanKey(3));
  });

  it("does not throw errors for keys no longer in the cache", () => {
    const cache = new IterableRecencyCache(2);
    cache.add(1, 'a');
    cache.add(2, 'b');
    cache.add(3, 'c');
    cache.add(4, 'd');
    cache.add(5, 'e');

    assert.doesNotThrow(() => cache.get(2));
    assert.doesNotThrow(() => cache.getAndRefresh(2));
  });
});