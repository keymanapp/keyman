export class IterableRecencyCache<Key, Value> {
  static readonly DEFAULT_MAX_SIZE = 5;

  private cacheOrderSet: Set<Key> = new Set();
  private transitionMap: Map<Key, Value> = new Map();

  private maxSize: number;

  constructor(maxSize: number = IterableRecencyCache.DEFAULT_MAX_SIZE) {
    this.maxSize = maxSize;
  }

  get count() {
    return this.cacheOrderSet.size;
  }

  get(key: Key): Value {
    const entry = this.transitionMap.get(key);
    return entry;
  }

  getAndRefresh(key: Key): Value {
    const cache = this.cacheOrderSet;
    if(!cache.has(key)) {
      return null;
    }

    cache.delete(key);
    cache.add(key);

    return this.get(key);
  }

  add(key: Key, value: Value) {
    const cache = this.cacheOrderSet;
    const map = this.transitionMap;

    if(cache.has(key)) {
      cache.delete(key);
      cache.add(key);
    } else {
      cache.add(key);

      if(cache.size > this.maxSize) {
        const oldest = cache.keys().next().value as Key;
        cache.delete(oldest);
        map.delete(oldest);
      }
    }

    this.transitionMap.set(key, value);
  }

  keys(): Key[] {
    return [...this.cacheOrderSet.keys()].reverse();
  }

  clear() {
    this.cacheOrderSet = new Set();
    this.transitionMap = new Map();
  }

  clearNewerThanKey(key: Key) {
    const cache = this.cacheOrderSet;
    const map = this.transitionMap;

    if(!cache.has(key)) {
      throw new Error("Key not found; cannot clear entries appearing afterward.");
    }

    const toClear: Key[] = [];
    let keyFound: boolean = false;
    for(const k of cache.keys()) {
      if(k == key) {
        keyFound = true;
        continue;
      }
      if(!keyFound) {
        continue;
      }

      toClear.push(k);
    }

    for(const k of toClear) {
      cache.delete(k);
      map.delete(k);
    }
  }
}