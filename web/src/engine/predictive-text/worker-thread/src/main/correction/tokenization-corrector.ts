import { PriorityQueue } from "@keymanapp/web-utils";

import { ContextToken } from "./context-token.js";
import { CorrectionSearchable, PathResult } from "./correction-searchable.js";
import { ContextTokenization } from "../test-index.js";
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

  private readonly _lockedTokens: ContextToken[];
  private readonly _boundTokens: ContextToken[];
  private _unboundToken?: ContextToken;

  private selectionQueue: PriorityQueue<ContextToken>;
  private tokenCostMap: Map<ContextToken, number>;
  private _lockedTokenResults: Map<ContextToken, TokenResult>;
  private lastTotalCost: number;

  get currentCost(): number {
    const token = this.selectionQueue.peek();
    if(!token) {
      return this.lastTotalCost;
    }

    return this.getUpdatedTotalCost(token, token.searchModule.currentCost);
  };

  get orderedTokens(): ReadonlyArray<ContextToken> {
    return this.tokenization.tokens.slice(-this.tailCorrectionLength);
  }

  get lockedTokens(): ReadonlyArray<ContextToken> {
    return this._lockedTokens;
  }

  get boundTokens(): ReadonlyArray<ContextToken> {
    return this._boundTokens;
  }

  get unboundToken(): ContextToken {
    return this._unboundToken;
  }

  get lockedTokenResults(): ReadonlyMap<ContextToken, TokenResult> {
    return this._lockedTokenResults;
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

    this._lockedTokens = [];
    this._boundTokens = [];

    correctables.forEach((token, index) => {
      if(!filterClosure(token)) {
        this._lockedTokens.push(token);
      } else if(index == tailCorrectionLength - 1) {
        this._unboundToken = token;
      } else {
        this._boundTokens.push(token);
      }
    });

    this._lockedTokenResults = new Map();
    const lockedTokens = this._lockedTokens;
    lockedTokens.forEach((t) => {
      const lockedResult = t.searchModule.bestExample;
      this._lockedTokenResults.set(t, {
        matchString: lockedResult.text,
        inputSamplingCost: 0,
        knownCost: -Math.log(lockedResult.p),
        totalCost: -Math.log(lockedResult.p)
      });
    });

    let totalCost = lockedTokens.reduce((accum, curr) => accum - Math.log(curr.searchModule.bestExample.p), 0);
    const tokenCostMap = this.tokenCostMap = new Map<ContextToken, number>();

    const tokensToQueue = this._boundTokens.concat(this.unboundToken ?? []);
    tokensToQueue.forEach((t) => {
      totalCost += t.searchModule.currentCost;
      tokenCostMap.set(t, t.searchModule.currentCost);
    });

    this.lastTotalCost = totalCost;

    // Compute a weighting for each token's search space based the increase in
    // tokenization cost that it represents.
    const tokenUpdateCost = (token: ContextToken) => token.searchModule.currentCost - (tokenCostMap.get(token) ?? 0)
    this.selectionQueue = new PriorityQueue((a, b) => {
      const aUpdateCost = tokenUpdateCost(a);
      const bUpdateCost = tokenUpdateCost(b);

      // Division or subtraction, we get the same effect for ordering:  the
      // operands are all positive.  Subtraction is computationally less costly.
      return aUpdateCost - bUpdateCost;
    });

    this.selectionQueue.enqueueAll(tokensToQueue);
  }

  private getUpdatedTotalCost(updatedToken: ContextToken, tokenCost: number): number {
    return this.lastTotalCost + tokenCost - (this.tokenCostMap.get(updatedToken) ?? 0);
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

    const tokenToUpdate = this.selectionQueue.dequeue();
    const tokenResult = tokenToUpdate.searchModule.handleNextNode();

    if(tokenResult.type == 'none') {
      // If we reach this point, the tokenization has exhausted its search space.
      return {
        'type': 'none'
      };
    }

    // Update the cost associated with the token.
    const cost = this.lastTotalCost = this.getUpdatedTotalCost(tokenToUpdate, tokenResult.cost);
    this.tokenCostMap.set(tokenToUpdate, tokenResult.cost);

    // If we haven't found a valid correction for the token with lowest-cost update,
    // just requeue it and keep searching until we find one.
    if(tokenResult.type != 'complete') {
      this.selectionQueue.enqueue(tokenToUpdate);

      // Needs to return the 'proper' type of result.
      return {
        type: 'intermediate',
        cost
      };
    }

    // Assertion:  tokenResult.type == 'complete'.  We have a valid correction for
    // at least some part of the tokenization - the represented context variant.
    if(tokenToUpdate != this.unboundToken) {
      // Lock the 'bound' token now that a valid correction for it has been
      // found. We only consider a single correction for most of a
      // tokenization's tokens, generally only allowing correction variation for
      // the last represented token.
      this._boundTokens.splice(this._boundTokens.indexOf(tokenToUpdate), 1);
      this._lockedTokens.push(tokenToUpdate);
    }

    // Either way, update the token -> correction-string map with the obtained result.
    this._lockedTokenResults.set(tokenToUpdate, tokenResult.mapping);

    // If we have a correction for all components in need of correction, allow
    // searching for alternative corrections for the 'unbound' token.
    if(this._boundTokens.length == 0 && this._unboundToken) {
      this.selectionQueue.enqueue(this._unboundToken);
    }

    const tokenCorrections: TokenResult[] = this.orderedTokens.map((t) => this._lockedTokenResults.get(t));

    if(tokenCorrections.findIndex((c) => c == undefined) != -1) {
      return {
        type: 'intermediate',
        cost
      }
    }

    // Determine the proper return type and construct the proper return object accordingly.
    // const resultMap = new Map(this.lockedTokenResults);
    return {
      type: 'complete',
      cost,
      mapping: new TokenizationResultMapping(tokenCorrections, this)
    };
  }
}