import { SearchNode, SearchResult } from "./distance-modeler.js";

/**
 * A helper class for ensuring that equally-likely corrections are returned
 * from the correction-search on even footing.
 *
 * Pre-conditions:
 * - entries should be examined in ascending-cost order.
 */
export class SearchBatcher {
  private currentCost = Number.MIN_SAFE_INTEGER;
  private entries: SearchResult[] = [];

  private currentReturns: {[mapKey: string]: SearchNode} = {};

  constructor() {
  }

  /**
   * Gets the cost of results currently batched together.
   * All added entries should have matching `batchCost`.
   */
  get batchCost(): number {
    return this.currentCost;
  }

  /**
   * Adds a new entry to the 'batch' currently under construction.  Its cost
   * should match `this.batchCost`.
   * @param entry
   */
  add(entry: SearchNode): void {
    // While not checked, the usage pattern intended for this class would assert that
    // `entry.currentCost == this.batchCost`.
    const currentReturns = this.currentReturns;
    if(!currentReturns[entry.resultKey]) {
      this.entries.push(new SearchResult(entry));
      currentReturns[entry.resultKey] = entry;
    }
  }

  /**
   * Finalizes the current batch, returning it if not empty.  Also updates
   * the `.batchCost` to use for the next attempt at batching.
   * @param nextCost
   * @returns
   */
  finalizeBatch(nextCost: number): SearchResult[] | null {
    this.currentCost = nextCost;

    if(this.entries.length == 0) {
      return null;
    }

    const result: SearchResult[] = this.entries;
    this.entries = [];
    return result;
  }
}