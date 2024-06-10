import { SearchNode, SearchResult } from "./distance-modeler.js";


export class SearchBatcher {
  currentCost = Number.MIN_SAFE_INTEGER;
  entries: SearchResult[] = [];

  // might should also include a 'base cost' parameter of sorts?
  searchCache: {
    [mapKey: string]: SearchNode;
  };
  currentReturns: {[mapKey: string]: SearchNode} = {};

  constructor(searchCache: typeof SearchBatcher.prototype.searchCache) {
    this.searchCache = searchCache;
  }

  checkAndAdd(entry: SearchNode): SearchResult[] | null {
    var result: SearchResult[] = null;

    if(entry.currentCost > this.currentCost) {
      result = this.tryFinalize();

      this.currentCost = entry.currentCost;
    }

    // Filter out any duplicated match sequences.  The same match sequence may be reached via
    // different input sequences, after all.
    let outputMapKey = entry.calculation.matchSequence.map(value => value.key).join('');

    const searchCache = this.searchCache;
    // First, ensure the edge has an existing 'shared' cache entry.
    if(!searchCache[outputMapKey]) {
      searchCache[outputMapKey] = entry;
    }

    const currentReturns = this.currentReturns;

    // Check the generator's local returned-value cache - this determines
    // whether or not we need to add a new 'return' to the batch.
    //
    // Is not necessarily the same as `searchCache`.  In particular, on
    // revisiting the context state, `searchCache` is used to rebuild
    // `currentReturns` in order to efficiently rebuild results that have
    // already been iterated through once.
    if(!currentReturns[outputMapKey]) {
      this.entries.push(new SearchResult(entry));
      currentReturns[outputMapKey] = entry;
    }

    return result;
  }

  tryFinalize(): SearchResult[] | null {
    var result: SearchResult[] = null;
    if(this.entries.length > 0) {
      result = this.entries;
      this.entries = [];
    }

    return result;
  }
}