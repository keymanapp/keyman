/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2026-04-02
 *
 * This file defines the `TokenizationCorrector` class, which is used to
 * prioritize optimal multi-token corrections (and predictions) within the
 * predictive-text correction-search engine.
 */

import { PriorityQueue } from "@keymanapp/web-utils";

import { ContextToken } from "./context-token.js";
import { CorrectionSearchable, PathResult } from "./correction-searchable.js";
import { ContextTokenization } from "./context-tokenization.js";
import { SearchQuotientNode } from "./search-quotient-node.js";
import { TokenizationResultMapping } from "./tokenization-result-mapping.js";

// PathResult needs to be generic:
// - a result for correcting a single Token - "TokenResult"?
// - a result for completing correction for a full Tokenization - "TokenizationResult"?

export type TokenResult = {
  matchString: string,
  inputSamplingCost: number,
  knownCost: number,
  totalCost: number
}

export class TokenizationCorrector implements CorrectionSearchable<ReadonlyArray<TokenResult>, TokenizationResultMapping> {
  public readonly tokenization: ContextTokenization;
  private readonly tailCorrectionLength: number

  private readonly _uncorrectables: SearchQuotientNode[];
  private readonly _correctables: SearchQuotientNode[];
  private _predictable?: SearchQuotientNode;

  private selectionQueue: PriorityQueue<SearchQuotientNode>;
  private tokenCostMap: Map<number, number>;
  private _lockedTokenResults: Map<SearchQuotientNode, TokenResult>;
  private lastTotalCost: number;
  private handleHasBeenCalled: boolean = false;

  get currentCost(): number {
    const correctable = this.selectionQueue.peek();
    if(!correctable) {
      return this.lastTotalCost;
    }

    return this.getUpdatedTotalCost(correctable, correctable.currentCost);
  };

  get orderedTokens(): ReadonlyArray<ContextToken> {
    return this.tokenization.tokens.slice(-this.tailCorrectionLength);
  }

  get uncorrectableTokens(): ReadonlyArray<ContextToken> {
    return this.orderedTokens.filter((t) => this._uncorrectables.find((c) => c.spaceId == t.spaceId));
  }

  get correctableTokens(): ReadonlyArray<ContextToken> {
    return this.orderedTokens.filter((t) => this._correctables.find((c) => c.spaceId == t.spaceId));
  }

  get predictableToken(): ContextToken {
    return this.orderedTokens.find((t) => this._predictable?.spaceId == t.spaceId);
  }

  get lockedTokenResults(): ReadonlyMap<ContextToken, TokenResult> {
    return new Map([...this._lockedTokenResults.entries()]
      .map((tuple) => [this.orderedTokens.find((t) => t.searchModule == tuple[0]), tuple[1]]));
  }

  // Will have actual result sequences.
  //
  // Once we have an actual answer for all non-locked tokens, the first entry
  // should appear.  The only variation among results, after that, should be the
  // correction for the last token.
  //
  // Results may be a clone of the lockedTokenResults map.  The owning
  // tokenization may then flesh out the ordering of the entries to build the
  // proper corrections / predictions.
  get previousResults(): TokenizationResultMapping[] {
    return [];
  };

  constructor(
    tokenization: ContextTokenization,
    tailCorrectionLength: number,
    filterClosure: (token: ContextToken) => boolean
  ) {
    this.tokenization = tokenization;
    this.tailCorrectionLength = tailCorrectionLength;

    if(tailCorrectionLength < 1) {
      throw new Error(`Length for correction near tail may not be 0.`);
    }

    const correctables = this.orderedTokens;

    this._uncorrectables = [];
    this._correctables = [];

    correctables.forEach((token, index) => {
      const searchModule = token.searchModule;
      if(!filterClosure(token)) {
        this._uncorrectables.push(searchModule);
      } else if(index == tailCorrectionLength - 1) {
        this._predictable = searchModule;
      } else {
        this._correctables.push(searchModule);
      }
    });

    this._lockedTokenResults = new Map();
    const uncorrectables = this._uncorrectables;
    uncorrectables.forEach((uncorrectable) => {
      const lockedResult = uncorrectable.bestExample;
      this._lockedTokenResults.set(uncorrectable, {
        matchString: lockedResult.text,
        inputSamplingCost: 0,
        knownCost: -Math.log(lockedResult.p),
        totalCost: -Math.log(lockedResult.p)
      });
    });

    let totalCost = uncorrectables.reduce((accum, curr) => accum - Math.log(curr.bestExample.p), 0);
    const tokenCostMap = this.tokenCostMap = new Map<number, number>();

    const correctablesToQueue = this._correctables.concat(this.predictableToken?.searchModule ?? []);
    correctablesToQueue.forEach((t) => {
      totalCost += t.currentCost;
      tokenCostMap.set(t.spaceId, t.currentCost);
    });

    this.lastTotalCost = totalCost;

    // Compute a weighting for each token's search space based the increase in
    // tokenization cost that it represents.
    const tokenUpdateCost = (searchModule: SearchQuotientNode) => searchModule.currentCost - (tokenCostMap.get(searchModule.spaceId) ?? 0)
    this.selectionQueue = new PriorityQueue((a, b) => {
      const aUpdateCost = tokenUpdateCost(a);
      const bUpdateCost = tokenUpdateCost(b);

      // Division or subtraction, we get the same effect for ordering:  the
      // operands are all positive.  Subtraction is computationally less costly.
      return aUpdateCost - bUpdateCost;
    });

    this.selectionQueue.enqueueAll(correctablesToQueue);
  }

  private getUpdatedTotalCost(updatedCorrectable: SearchQuotientNode, tokenCost: number): number {
    return this.lastTotalCost + tokenCost - (this.tokenCostMap.get(updatedCorrectable.spaceId) ?? 0);
  }

  private collateResults(): TokenizationResultMapping {
    return new TokenizationResultMapping(this.orderedTokens.map((t) => this._lockedTokenResults.get(t.searchModule)), this);
  }

  handleNextNode(): PathResult<TokenizationResultMapping> {
    // Notable states:
    // 1.  Unbound tokens have not yet been "locked" - no valid correction has yet been found.
    //     - Variation:  the final, "unbound" token may be locked while awaiting this case.
    //     - If so, remember the corresponding matchString!
    // 2.  An unbound token may become "locked" - a workable correction is found.
    //     - Remember the correction's matchString / correction!
    // 3.  The **last** unbound token may finally become "locked".
    //     - If final "unbound" token is locked, unlock it!
    //     - Produce first search result!
    // 4.  Unbound token is unlocked, but all others are locked.

    const correctableToUpdate = this.selectionQueue.dequeue();
    const tokenResult = correctableToUpdate?.handleNextNode();

    if(tokenResult?.type == 'none' || (!tokenResult && this.handleHasBeenCalled)) {
      // If we reach this point, the tokenization has exhausted its search space.
      return {
        'type': 'none'
      };
    } else if(!tokenResult) {
      this.handleHasBeenCalled = true;
      return {
        'type': 'complete',
        cost: this.lastTotalCost,
        mapping: this.collateResults()
      };
    }

    this.handleHasBeenCalled = true;

    // Update the cost associated with the token.
    const cost = this.lastTotalCost = this.getUpdatedTotalCost(correctableToUpdate, tokenResult.cost);
    this.tokenCostMap.set(correctableToUpdate.spaceId, tokenResult.cost);

    // If we haven't found a valid correction for the token with lowest-cost update,
    // just requeue it and keep searching until we find one.
    if(tokenResult.type != 'complete') {
      this.selectionQueue.enqueue(correctableToUpdate);

      // Needs to return the 'proper' type of result.
      return {
        type: 'intermediate',
        cost
      };
    }

    // Assertion:  tokenResult.type == 'complete'.  We have a valid correction for
    // at least some part of the tokenization - the represented context variant.
    if(correctableToUpdate != this._predictable) {
      // Lock the 'bound' token now that a valid correction for it has been
      // found. We only consider a single correction for most of a
      // tokenization's tokens, generally only allowing correction variation for
      // the last represented token.
      this._correctables.splice(this._correctables.indexOf(correctableToUpdate), 1);
      this._uncorrectables.push(correctableToUpdate);
    }

    // Either way, update the token -> correction-string map with the obtained result.
    this._lockedTokenResults.set(correctableToUpdate, tokenResult.mapping);

    // If we have a correction for all components in need of correction, allow
    // searching for alternative corrections for the 'unbound' token.
    if(this._correctables.length == 0 && this._predictable) {
      this.selectionQueue.enqueue(this._predictable);
    }

    const correctionResults = this.collateResults();
    if(correctionResults.matchedResult.findIndex((c) => c == undefined) != -1) {
      return {
        type: 'intermediate',
        cost
      };
    }

    // Determine the proper return type and construct the proper return object accordingly.
    // const resultMap = new Map(this.lockedTokenResults);
    return {
      type: 'complete',
      cost,
      mapping: correctionResults
    };
  }
}