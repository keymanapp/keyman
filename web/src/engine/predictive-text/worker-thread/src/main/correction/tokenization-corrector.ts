/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2026-04-02
 *
 * This file defines the `TokenizationCorrector` class, which is used to
 * prioritize optimal multi-token corrections (and predictions) within the
 * predictive-text correction-search engine.
 */

import { PriorityQueue } from "keyman/common/web-utils";

import { ContextToken } from "./context-token.js";
import { CorrectionSearchable, PathResult } from "./correction-searchable.js";
import { ContextTokenization } from "./context-tokenization.js";
import { QuotientNodeFinalizer } from "./quotient-node-finalizer.js";
import { TokenizationResult, TokenizationResultMapping } from "./tokenization-result-mapping.js";
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
  inputCount: number,
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
export class TokenizationCorrector implements CorrectionSearchable<TokenizationResult, TokenizationResultMapping> {
  /**
   * The root ContextTokenization all generated corrections are based upon.
   */
  public readonly tokenization: ContextTokenization;

  /**
   * Indicates whether or not the correction range of this TokenizationCorrector
   * started with any tokens considered "correctable".
   */
  public readonly modelsCorrectables: boolean;

  // public read-only via properties
  private readonly _uncorrectables: QuotientNodeFinalizer[];
  private readonly _correctables: QuotientNodeFinalizer[];
  private _predictable?: QuotientNodeFinalizer;
  private _generatedTokenResults: Map<number, TokenResult>;
  private _previousResults: TokenizationResultMapping[] = [];
  private _correctableCodepoints: number = 0;
  private _correctablesMatched = 0;

  // fully private
  private selectionQueue: PriorityQueue<QuotientNodeFinalizer>;
  private tokenCostMap: Map<number, number>;
  private tokenLookupMap: Map<number, ContextToken>;
  private lastTotalCost: number;
  private handleHasBeenCalled: boolean = false;
  private predictableMatchFound: boolean = false;
  private matchableTokenCount = 0;
  private readonly tailCorrectionLength: number;

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
   * Note that some tokens may have started out as "correctable" initially, becoming "uncorrectable" after
   * their first viable correction was found.
   *
   * Other tokens may have been labeled "uncorrectable" from the start.
   */
  get uncorrectableTokens(): ReadonlyArray<ContextToken> {
    return this._uncorrectables.map((c) => this.tokenLookupMap.get(c.spaceId));
  }

  /**
   * Returns the tokens, in order, that are considered "correctable".
   *
   * Correction-search will search for only the first viable correction for each
   * and will penalize any additional codepoints not already within the token
   * that prove necesssary to match a valid lexical entry.
   */
  get correctableTokens(): ReadonlyArray<ContextToken> {
    return this._correctables.map((c) => this.tokenLookupMap.get(c.spaceId));
  }

  get correctableCodepoints(): number {
    return this._correctableCodepoints;
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
    return this.tokenLookupMap.get(this._predictable?.spaceId);
  }

  /**
   * Returns the current map of token-to-corrections that has been determined thus far.
   *
   * Tokens initially considered "uncorrectable" will have valid, pre-set entries.
   */
  get generatedTokenResults(): ReadonlyMap<ContextToken, TokenResult> {
    return new Map([...this._generatedTokenResults.entries()]
      .map((tuple) => [this.tokenLookupMap.get(tuple[0]), tuple[1]]));
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

  get matchedTokenCount() {
    return this._correctablesMatched + (this.predictableMatchFound ? 1 : 0);
  }

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
    filterClosure: (token: ContextToken, index?: number) => boolean
  ) {
    this.tokenization = tokenization;
    this.tailCorrectionLength = tailCorrectionLength;

    if(tailCorrectionLength < 1) {
      throw new Error(`Length for correction near tail may not be ${tailCorrectionLength} - it must be a positive number.`);
    } else if(tailCorrectionLength > tokenization.tokens.length) {
      throw new Error(`Tail correction length must not extend actual token count - ${tailCorrectionLength} > ${tokenization.tokens.length}`);
    }

    const orderedTokens = this.orderedTokens;

    this._uncorrectables = [];
    this._correctables = [];

    this.tokenLookupMap = new Map();
    let modelsCorrectables = false;

    // 0 index:  the first index in range to be modeled, as split off from the main tokenization.
    orderedTokens.forEach((token, index) => {
      // New issue:  this mangles the space IDs!  We almost certainly need some
      // sort of proper map to the source token.
      const searchModule = new QuotientNodeFinalizer(token.searchModule, index == orderedTokens.length - 1);
      this.tokenLookupMap.set(searchModule.spaceId, token);
      // Index within the token subset being examined.
      const passesFilter = filterClosure(token, index);
      modelsCorrectables ||= passesFilter;
      if(!passesFilter) {
        this._uncorrectables.push(searchModule);
        return;
      }

      this.matchableTokenCount++;
      this._correctableCodepoints += searchModule.codepointLength;
      if(index == tailCorrectionLength - 1) {
        // The sole assignment case for this field.  It may only be assigned for
        // the final token, and only if its text is of a form considered
        // correctable by the filter.
        this._predictable = searchModule;
      } else {
        this._correctables.push(searchModule);
      }
    });
    // Set a readonly flag indicating if this Corrector started with correctable entries.
    this.modelsCorrectables = modelsCorrectables;

    this._generatedTokenResults = new Map();
    const uncorrectables = this._uncorrectables;
    uncorrectables.forEach((uncorrectable) => {
      const lockedResult = uncorrectable.bestExample;
      this._generatedTokenResults.set(uncorrectable.spaceId, {
        matchString: lockedResult.text,
        inputSamplingCost: -Math.log(lockedResult.p),
        inputCount: uncorrectable.inputCount,
        knownCost: 0,
        totalCost: -Math.log(lockedResult.p)
      });
    });

    let totalCost = uncorrectables.reduce((accum, curr) => accum - Math.log(curr.bestExample.p), 0);
    const tokenCostMap = this.tokenCostMap = new Map<number, number>();

    const correctablesToQueue = this._correctables.concat(this._predictable ?? []);
    correctablesToQueue.forEach((t) => {
      totalCost += t.currentCost;
      tokenCostMap.set(t.spaceId, t.currentCost);
    });

    this.lastTotalCost = totalCost;

    // Compute a weighting for each token's search space based the increase in
    // tokenization cost that it represents.
    const tokenUpdateCost = (searchModule: QuotientNodeFinalizer) => searchModule.currentCost - (tokenCostMap.get(searchModule.spaceId) ?? 0)
    this.selectionQueue = new PriorityQueue((a, b) => {
      const aUpdateCost = tokenUpdateCost(a);
      const bUpdateCost = tokenUpdateCost(b);

      // Division or subtraction, we get the same effect for ordering:  the
      // operands are all positive.  Subtraction is computationally less costly.
      return aUpdateCost - bUpdateCost;
    });

    this.selectionQueue.enqueueAll(correctablesToQueue);
  }

  private getUpdatedTotalCost(updatedCorrectable: QuotientNodeFinalizer, tokenCost: number): number {
    return this.lastTotalCost + tokenCost - (this.tokenCostMap.get(updatedCorrectable.spaceId) ?? 0);
  }

  /**
   * Converts the internal 'generated token results' map into the proper Tokenization-correction return type.
   */
  private collateResults(): TokenizationResultMapping {
    // The tokenLookupMap was constructed in the same ordering as the tokens; we can iterate the keys
    // or entries to keep everything in order.
    const results = [...this.tokenLookupMap.keys()].map((spaceId) => this._generatedTokenResults.get(spaceId))
    return new TokenizationResultMapping(results, this);
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

        // If no matchables exist, there's no prediction to do; don't make a return.
        if(this.matchedTokenCount > 0) {
          const results = this.collateResults();
          this._previousResults.push(results);
          return {
            'type': 'complete',
            cost: this.lastTotalCost,
            mapping: results
          };
        } else {
          return { type: 'none' };
        }
      }
    }

    this.handleHasBeenCalled = true;

    const correctableToUpdate = this.selectionQueue.dequeue();
    const tokenResult = correctableToUpdate?.handleNextNode();

    const correctionIsThePredictable = correctableToUpdate == this._predictable;
    const delistCorrectable = () => {
      if(!correctionIsThePredictable) {
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
      // If it's a correction, or if we were unable to find a correction for
      // the predictable token - both cases need a 'default 'entry.
      if(!correctionIsThePredictable || !this.predictableMatchFound) {
        // Transition the node from 'correctable' to 'uncorrectable' - we were
        // unable to find valid corrections for it.
        const lockedResult = correctableToUpdate.bestExample;
        this._generatedTokenResults.set(correctableToUpdate.spaceId, {
          matchString: lockedResult.text,
          inputSamplingCost: -Math.log(lockedResult.p),
          inputCount: correctableToUpdate.inputCount,
          knownCost: MAX_EDIT_THRESHOLD_FACTOR, // we'll use the same threshold at which further search is terminated.
          totalCost: -Math.log(lockedResult.p) + MAX_EDIT_THRESHOLD_FACTOR * EDIT_DISTANCE_COST_SCALE
        });
      }

      this._correctableCodepoints -= correctableToUpdate.codepointLength;

      // We can make no further predictions if we've exhausted all search options.
      // If we've reached this case, we're likely at the end of the search
      // (unless correction for a correctable is still possible).
      if(correctionIsThePredictable) {
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

      if(correctionIsThePredictable) {
        this.predictableMatchFound = true;
      } else {
        this._correctablesMatched++;
      }

      // Either way, update the token -> correction-string map with the obtained result.
      this._generatedTokenResults.set(correctableToUpdate.spaceId, tokenResult.mapping);
    }

    const resultCost = tokenResult.type != 'none' ? tokenResult.cost : this._generatedTokenResults.get(correctableToUpdate.spaceId).totalCost;

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

    // If any token lacks a matching lookup value, abort.
    if([...this.tokenLookupMap.keys()].find((k) => !this._generatedTokenResults.has(k))) {
      return {
        type: 'intermediate',
        cost: tokenizationCost
      };
    }

    // Determine the proper return type and construct the proper return object accordingly.
    //
    // If there was no result obtained from the predictable and a result was previously found,
    // that indicates no further predictions may be found.
    if(tokenResult.type != 'none' || !correctionIsThePredictable || !this.predictableMatchFound) {
      if(this.matchedTokenCount > 0) {
        const correctionResults = this.collateResults();
        this._previousResults.push(correctionResults);
        return {
          type: 'complete',
          cost: tokenizationCost,
          mapping: correctionResults
        };
      } else {
        return {
          type: 'none'
        }
      }
    } else {
      return {
        type: 'none'
      };
    }
  }
}