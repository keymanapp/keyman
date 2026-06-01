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
export class RewindableCache<Value> {
  static readonly DEFAULT_MAX_SIZE = 5;

  private map: Map<number, Value> = new Map();
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
    return this.map.size;
  }

  /**
   * Fetches the cache entry with the specified key without updating its
   * most recent access time.
   * @param key
   * @returns
   */
  get(key: number): Value {
    const entry = this.map.get(key);
    return entry;
  }

  /**
   * Adds the specified key-value pair as a cache entry, overwriting any
   * previous value for the specified key.
   * @param key
   * @returns
   */
  add(key: number, value: Value) {
    const map = this.map;

    if(map.has(key)) {
      map.delete(key);
      map.set(key, value);
    } else {
      map.set(key, value);

      if(map.size > this.maxSize) {
        const oldest = map.keys().next().value as number;
        map.delete(oldest);
      }
    }
  }

  /**
   * Returns an array of all cache key entries according to their last access time -
   * from most recent to least recent.
   *
   * Note:  returns an Array and is accordingly O(N).
   * @returns
   */
  keys(): number[] {
    return [...this.map.keys()].reverse();
  }

  /**
   * Deletes all entries from the cache.
   */
  clear() {
    this.map = new Map();
  }

  /**
   * Deletes all entries with a key greater than the specified key (which indicates
   * the entry was first created more recently), leaving the specified key as
   * the most recent entry.
   * @param key
   */
  rewindTo(key: number) {
    const map = this.map;

    if(!map.has(key)) {
      throw new Error("Key not found; cannot clear entries appearing afterward.");
    }

    // Iteration:  by insertion order
    // The most recent entries will appear after the specified key.
    for(const k of map.keys()) {
      if(k > key) {
        // It is safe to delete a key from a Set or Map while iterating over it in ES6+.
        // https://stackoverflow.com/a/28306768
        map.delete(k);
      }
    }
  }
}