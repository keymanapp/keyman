import { CorrectionResultMapping } from "./correction-result-mapping.js";
import { TokenizationCorrector, TokenResult } from './tokenization-corrector.js';

export class TokenizationResultMapping implements CorrectionResultMapping<ReadonlyArray<TokenResult>> {
  readonly matchingSpace: TokenizationCorrector;
  readonly matchedResult: ReadonlyArray<TokenResult>;

  constructor(tokenization: TokenResult[], corrector: TokenizationCorrector) {
    this.matchingSpace = corrector;
    this.matchedResult = tokenization;
  }

  get spaceId(): number {
    return this.matchingSpace.tokenization.spaceId;
  }

  // /**
  //  * Gets the number of Damerau-Levenshtein edits needed to reach the node's
  //  * matchString from the output induced by the input sequence used to reach it.
  //  *
  //  * (This is scaled by `SearchSpace.EDIT_DISTANCE_COST_SCALE` when included in
  //  * `totalCost`.)
  //  */
  // get knownCost(): number {
  //   return this.node.editCount;
  // }

  // /**
  //  * Gets the "input sampling cost" of the edge, which should be considered as the
  //  * negative log-likelihood of the input path taken to reach the node.
  //  */
  // get inputSamplingCost(): number {
  //   return this.node.inputSamplingCost;
  // }

  /**
   * Gets the "total cost" of the edge, which should be considered as the
   * negative log-likelihood of the input path taken to reach the node
   * multiplied by the 'probability' induced by needed Damerau-Levenshtein edits
   * to the resulting output.
   */
  get totalCost(): number {
    return this.matchedResult.reduce((total, curr) => total + curr.totalCost, 0);
  }
}