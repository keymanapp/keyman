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
  public readonly orderedTokens: ReadonlyArray<ContextToken>;

  private readonly lockedTokens: ContextToken[];
  private readonly boundTokens: ContextToken[];
  private unboundToken?: ContextToken;

  private selectionQueue: PriorityQueue<ContextToken>;
  private tokenCostMap: Map<ContextToken, number>;
  private lockedTokenResults: Map<ContextToken, TokenResult>;
  private lastTotalCost: number;

  get currentCost(): number {
    const token = this.selectionQueue.peek();
    if(!token) {
      return this.lastTotalCost;
    }

    return this.getUpdatedTotalCost(token);
  };

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
    orderedTokens: ContextToken[],
    lockedTokens: ContextToken[],
    boundTokens: ContextToken[],
    unboundToken?: ContextToken
  ) {
    this.tokenization = tokenization;
    this.orderedTokens = orderedTokens;

    this.lockedTokens = lockedTokens;
    this.boundTokens = boundTokens;
    this.unboundToken = unboundToken;

    this.lockedTokenResults = new Map();
    lockedTokens.forEach((t) => {
      const lockedResult = t.searchModule.bestExample;
      this.lockedTokenResults.set(t, {
        matchString: lockedResult.text,
        inputSamplingCost: 0,
        knownCost: -Math.log(lockedResult.p),
        totalCost: -Math.log(lockedResult.p)
      });
    });

    let totalCost = lockedTokens.reduce((accum, curr) => accum * curr.searchModule.bestExample.p, 0);
    const tokenCostMap = this.tokenCostMap = new Map<ContextToken, number>();

    const tokensToQueue = boundTokens.concat(unboundToken);
    tokensToQueue.forEach((t) => {
      totalCost *= t.searchModule.currentCost;
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

  private getUpdatedTotalCost(updatedToken: ContextToken): number {
    return this.lastTotalCost * updatedToken.searchModule.currentCost / (this.tokenCostMap.get(updatedToken) ?? 1);
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
      return null;
    }

    // Update the cost associated with the token.
    this.tokenCostMap.set(tokenToUpdate, tokenResult.cost);
    this.lastTotalCost = this.getUpdatedTotalCost(tokenToUpdate);

    // If we haven't found a valid correction for the token with lowest-cost update,
    // just requeue it and keep searching until we find one.
    if(tokenResult.type != 'complete') {
      this.selectionQueue.enqueue(tokenToUpdate);

      // Needs to return the 'proper' type of result.
      return null;
    }

    // Assertion:  tokenResult.type == 'complete'.  We have a valid correction for
    // at least some part of the tokenization - the represented context variant.
    if(tokenToUpdate != this.unboundToken) {
      // Lock the 'bound' token now that a valid correction for it has been
      // found. We only consider a single correction for most of a
      // tokenization's tokens, generally only allowing correction variation for
      // the last represented token.
      this.boundTokens.splice(this.boundTokens.indexOf(tokenToUpdate), 1);
      this.lockedTokens.push(tokenToUpdate);
    }

    // Either way, update the token -> correction-string map with the obtained result.
    this.lockedTokenResults.set(tokenToUpdate, tokenResult.mapping);

    // If we have a correction for all components in need of correction, allow
    // searching for alternative corrections for the 'unbound' token.
    if(this.boundTokens.length == 0) {
      this.selectionQueue.enqueue(tokenToUpdate);
    }

    // Determine the proper return type and construct the proper return object accordingly.
    // const resultMap = new Map(this.lockedTokenResults);
    return null;
  }
}