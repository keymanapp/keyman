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
import { EDIT_DISTANCE_COST_SCALE } from "./distance-modeler.js";
import { MAX_EDIT_THRESHOLD_FACTOR } from "./search-quotient-spur.js";

// PathResult needs to be generic:
// - a result for correcting a single Token - "TokenResult"?
// - a result for completing correction for a full Tokenization - "TokenizationResult"?

/**
 * Implements an interface (extended by TokenResultMapping) that represents the
 * form (and related probability data) of a token to be utilized for generation
 * of predictions.
 *
 * Notably, this can be instantiated directly from a token without use of
 * correction-search while still adhering to an interface compatible with
 * correction results.
 */
export type TokenResult = {
  matchString: string,
  inputSamplingCost: number,
  knownCost: number,
  totalCost: number
}

/**
 * This class is the focal point for support of whitespace and word-boundary
 * correction.  It uses the SearchQuotientNode search-spaces of an existing
 * tokenization's tokens to optimally prioritize the correction process among
 * all correctable tokens, generating corrections for the full represented
 * range.
 */
export class TokenizationCorrector implements CorrectionSearchable<ReadonlyArray<TokenResult>, TokenizationResultMapping> {
  public readonly tokenization: ContextTokenization;
  private readonly tailCorrectionLength: number;

  // public read-only via properties
  private readonly _uncorrectables: SearchQuotientNode[];
  private readonly _correctables: SearchQuotientNode[];
  private _predictable?: SearchQuotientNode;
  private _generatedTokenResults: Map<SearchQuotientNode, TokenResult>;
  private _previousResults: TokenizationResultMapping[] = [];

  // fully private
  private selectionQueue: PriorityQueue<SearchQuotientNode>;
  private tokenCostMap: Map<number, number>;
  private lastTotalCost: number;
  private handleHasBeenCalled: boolean = false;

  get currentCost(): number {
    const correctable = this.selectionQueue.peek();
    if(!correctable) {
      return this.lastTotalCost;
    }

    return this.getUpdatedTotalCost(correctable, correctable.currentCost);
  };

  /**
   * Returns the tokens contributing in some manner to correction-search and its weightings.
   */
  get orderedTokens(): ReadonlyArray<ContextToken> {
    return this.tokenization.tokens.slice(-this.tailCorrectionLength);
  }

  /**
   * Returns the tokens, in order, that are considered "uncorrectable"; correction-search will not
   * perform no further text-correction on them.
   *
   * Note that some tokens may have started out this way initially, becoming "uncorrectable" after
   * their first viable correction was found.
   *
   * Other tokens may have been labeled "uncorrectable" from the start.
   */
  get uncorrectableTokens(): ReadonlyArray<ContextToken> {
    return this.orderedTokens.filter((t) => this._uncorrectables.find((c) => c.spaceId == t.spaceId));
  }

  /**
   * Returns the tokens, in order, that are considered "correctable".
   *
   * Correction-search will search for only the first viable correction for each
   * and will penalize any additional codepoints not already within the token
   * that prove necesssary to match a valid lexical entry.
   */
  get correctableTokens(): ReadonlyArray<ContextToken> {
    return this.orderedTokens.filter((t) => this._correctables.find((c) => c.spaceId == t.spaceId));
  }

  /**
   * Returns the token, if it exists, that is considered "predictable".
   *
   * Correction-search will search for any number of corrections for this token
   * provided that the list of "correctables" has been exhausted.  If the list
   * of "correctables" still has entries, once an initial correction is found
   * for this token, correction will be suspended until the correctables list
   * is empty.
   */
  get predictableToken(): ContextToken {
    return this.orderedTokens.find((t) => this._predictable?.spaceId == t.spaceId);
  }

  /**
   * Returns the current map of token-to-corrections that has been determined thus far.
   *
   * Tokens initially considered "uncorrectable" will have valid, pre-set entries.
   */
  get generatedTokenResults(): ReadonlyMap<ContextToken, TokenResult> {
    return new Map([...this._generatedTokenResults.entries()]
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
    return this._previousResults;
  };

  /**
   * Constructs an instance of TokenizationCorrector for finding corrections for
   * correctable tokens within the specified section of an existing
   * ContextTokenization.
   * @param tokenization   The tokenization pattern under consideration,
   * containing tokens that may be correctable
   * @param tailCorrectionLength  The length, in tokens, at the end of the
   * tokenization pattern that should be considered for correction
   * @param filterClosure  A closure that indicates via boolean whether to permit correction
   * for each token
   */
  constructor(
    tokenization: ContextTokenization,
    tailCorrectionLength: number,
    filterClosure: (token: ContextToken) => boolean
  ) {
    this.tokenization = tokenization;
    this.tailCorrectionLength = tailCorrectionLength;

    if(tailCorrectionLength < 1) {
      throw new Error(`Length for correction near tail may not be ${tailCorrectionLength} - it must be a positive number.`);
    } else if(tailCorrectionLength > tokenization.tokens.length) {
      throw new Error(`Tail correction length must not extend actual token count - ${tailCorrectionLength} > ${tokenization.tokens.length}`);
    }

    const correctables = this.orderedTokens;

    this._uncorrectables = [];
    this._correctables = [];

    correctables.forEach((token, index) => {
      const searchModule = token.searchModule;
      if(!filterClosure(token)) {
        this._uncorrectables.push(searchModule);
      } else if(index == tailCorrectionLength - 1) {
        // The sole assignment case for this field.  It may only be assigned for
        // the final token, and only if its text is of a form considered
        // correctable by the filter.
        this._predictable = searchModule;
      } else {
        this._correctables.push(searchModule);
      }
    });

    this._generatedTokenResults = new Map();
    const uncorrectables = this._uncorrectables;
    uncorrectables.forEach((uncorrectable) => {
      const lockedResult = uncorrectable.bestExample;
      this._generatedTokenResults.set(uncorrectable, {
        matchString: lockedResult.text,
        inputSamplingCost: -Math.log(lockedResult.p),
        knownCost: 0,
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

    // Compute a weighting for each token's search space based upon the increase
    // in tokenization cost that it represents.
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

  /**
   * Converts the internal 'generated token results' map into the proper Tokenization-correction return type.
   */
  private collateResults(): TokenizationResultMapping {
    return new TokenizationResultMapping(this.orderedTokens.map((t) => this._generatedTokenResults.get(t.searchModule)), this);
  }

  // The actual method used to iteratively search for tokenization-level corrections.
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

    if(this.selectionQueue.count == 0) {
      // If we reach this point, the tokenization has exhausted its search space.
      if(this.handleHasBeenCalled) {
        return { type: 'none' };
      } else {
        // It is possible that the editable tokenization range exists entirely of
        // tokens considered to be uncorrectable.
        this.handleHasBeenCalled = true;
        const results = this.collateResults();
        this._previousResults.push(results);
        return {
          'type': 'complete',
          cost: this.lastTotalCost,
          mapping: results
        };
      }
    }

    this.handleHasBeenCalled = true;

    const correctableToUpdate = this.selectionQueue.dequeue();
    const tokenResult = correctableToUpdate?.handleNextNode();

    const delistCorrectable = () => {
      if(correctableToUpdate != this._predictable) {
        // Lock the 'correctable' token now that either a valid correction for
        // it has been found or all possible corrections are exhausted. We only
        // consider a single correction for most of a tokenization's tokens,
        // generally only allowing correction variation for the last represented
        // token.
        this._correctables.splice(this._correctables.indexOf(correctableToUpdate), 1);
        this._uncorrectables.push(correctableToUpdate);
      }
    }

    if(tokenResult.type == 'none') {
      // Transition the node from 'correctable' to 'uncorrectable' - we were
      // unable to find valid corrections for it.
      const lockedResult = correctableToUpdate.bestExample;
      this._generatedTokenResults.set(correctableToUpdate, {
        matchString: lockedResult.text,
        inputSamplingCost: -Math.log(lockedResult.p),
        knownCost: MAX_EDIT_THRESHOLD_FACTOR, // we'll use the same threshold at which further search is terminated.
        totalCost: -Math.log(lockedResult.p) + MAX_EDIT_THRESHOLD_FACTOR * EDIT_DISTANCE_COST_SCALE
      });

      // We can make no further predictions if we've exhausted all search options.
      if(correctableToUpdate == this._predictable) {
        this._uncorrectables.push(correctableToUpdate);
        delete this._predictable;
      } else {
        delistCorrectable();
      }
    } else if(tokenResult.type == 'complete') {
      // Note that at this stage, we do not requeue the 'predictable' - other
      // correctables may exist and need their first corrections before we look
      // for other corrective variations of the 'predictable'.
      delistCorrectable();

      // Either way, update the token -> correction-string map with the obtained result.
      this._generatedTokenResults.set(correctableToUpdate, tokenResult.mapping);
    }

    const resultCost = tokenResult.type != 'none' ? tokenResult.cost : this._generatedTokenResults.get(correctableToUpdate).totalCost;

    // Update the cost associated with the token.
    const tokenizationCost = this.lastTotalCost = this.getUpdatedTotalCost(correctableToUpdate, resultCost);
    this.tokenCostMap.set(correctableToUpdate.spaceId, resultCost);

    // If we haven't found a valid correction for the token with lowest-cost update,
    // just requeue it and keep searching until we find one.
    if(tokenResult.type == 'intermediate') {
      this.selectionQueue.enqueue(correctableToUpdate);

      // Needs to return the 'proper' type of result.
      return {
        type: 'intermediate',
        cost: tokenizationCost
      };
    }

    // If we have a correction for all components in need of correction, then
    // search for alternative corrections for the 'predictable' token - even if
    // we previously stopped searching for more because we found its first
    // correction before finding one for at least one other 'correctable'.
    if(this._correctables.length == 0 && this.selectionQueue.count == 0 && this._predictable) {
      this.selectionQueue.enqueue(this._predictable);
    }

    const correctionResults = this.collateResults();
    if(correctionResults.matchedResult.findIndex((c) => c == undefined) != -1) {
      return {
        type: 'intermediate',
        cost: tokenizationCost
      };
    }

    // Determine the proper return type and construct the proper return object accordingly.
    this._previousResults.push(correctionResults);
    return {
      type: 'complete',
      cost: tokenizationCost,
      mapping: correctionResults
    };
  }
}