/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2025-08-01
 *
 * Implements a 'rewindable cache' designed for use with Transcriptions
 * and context-transition events.  The cache can clear all entries more
 * recent than a specified entry while otherwise pushing old entries out
 * when the cached history grows too large.
 */

/**
 * Defines a Map-like cache that forgets the least-recent entry when
 * reaching a specified capacity and that provides the ability to
 * forget _all_ entries more recent than a specified entry.
 */
export class RewindableCache<Key, Value> {
  static readonly DEFAULT_MAX_SIZE = 5;

  private cacheOrderSet: Set<Key> = new Set();
  private transitionMap: Map<Key, Value> = new Map();

  private maxSize: number;

  /**
   * Creates an instance with the specified maximum capacity.
   * @param maxSize
   */
  constructor(maxSize: number = RewindableCache.DEFAULT_MAX_SIZE) {
    this.maxSize = maxSize;
  }

  /**
   * Gets the current number of entries in the cache.
   */
  get size() {
    return this.cacheOrderSet.size;
  }

  /**
   * Fetches the cache entry with the specified key without updating its
   * most recent access time.
   * @param key
   * @returns
   */
  peek(key: Key): Value {
    const entry = this.transitionMap.get(key);
    return entry;
  }

  /**
   * Fetches the cache entry with the specified key and also updates its
   * most recent access time.
   * @param key
   * @returns
   */
  get(key: Key): Value {
    const set = this.cacheOrderSet;
    if(!set.has(key)) {
      return null;
    }

    set.delete(key);
    set.add(key);

    return this.peek(key);
  }

  /**
   * Adds the specified key-value pair as a cache entry, overwriting any
   * previous value for the specified key.
   * @param key
   * @returns
   */
  add(key: Key, value: Value) {
    const set = this.cacheOrderSet;
    const map = this.transitionMap;

    if(set.has(key)) {
      set.delete(key);
      set.add(key);
    } else {
      set.add(key);

      if(set.size > this.maxSize) {
        const oldest = set.keys().next().value as Key;
        set.delete(oldest);
        map.delete(oldest);
      }
    }

    map.set(key, value);
  }

  /**
   * Returns an array of all cache key entries according to their last access time -
   * from most recent to least recent.
   *
   * Note:  returns an Array and is accordingly O(N).
   * @returns
   */
  keys(): Key[] {
    return [...this.cacheOrderSet.keys()].reverse();
  }

  /**
   * Deletes all entries from the cache.
   */
  clear() {
    this.cacheOrderSet = new Set();
    this.transitionMap = new Map();
  }

  /**
   * Deletes all entries more recent than the specified key, leaving
   * the specified key as the most recent entry.
   * @param key
   */
  rewindTo(key: Key) {
    const set = this.cacheOrderSet;
    const map = this.transitionMap;

    if(!set.has(key)) {
      throw new Error("Key not found; cannot clear entries appearing afterward.");
    }

    let keyFound: boolean = false;
    // Iteration:  by insertion order
    // The most recent entries will appear after the specified key.
    for(const k of set.keys()) {
      if(!keyFound) {
        keyFound = k == key;
        continue;
      }

      // It is safe to delete a key from a Set while iterating over it in ES6+.
      // https://stackoverflow.com/a/28306768
      set.delete(k);
      map.delete(k);
    }
  }
}