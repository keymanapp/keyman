import { CorrectionResultMapping } from "./correction-result-mapping.js";
import { TokenizationCorrector, TokenResult } from './tokenization-corrector.js';

export interface TokenizationResult {
  tokenCorrections: ReadonlyArray<TokenResult>,
  totalEditCount: number,
  totalEditableCodepoints: number
}

export class TokenizationResultMapping implements CorrectionResultMapping<TokenizationResult> {
  readonly matchingSpace: TokenizationCorrector;
  readonly matchedResult: TokenizationResult;

  constructor(tokenization: TokenResult[], corrector?: TokenizationCorrector) {
    this.matchingSpace = corrector;

    this.matchedResult = {
      tokenCorrections: tokenization,
      totalEditCount: tokenization.reduce((accum, curr) => accum + curr.knownCost, 0),
      // If based on a legacy/custom model not using traversals, we don't
      // support edit operations (for correction) beyond the direct results of
      // the most recent input distribution.
      totalEditableCodepoints: corrector?.correctableCodepoints ?? 0
    }
  }

  get spaceId(): number {
    return this.matchingSpace?.tokenization.spaceId;
  }

  /**
   * Gets the number of Damerau-Levenshtein edits needed to reach the node's
   * matchString from the output induced by the input sequence used to reach it.
   *
   * (This is scaled by `SearchSpace.EDIT_DISTANCE_COST_SCALE` when included in
   * `totalCost`.)
   */
  get knownCost(): number {
    return this.matchedResult.totalEditCount;
  }

  /**
   * Gets the "input sampling cost" of the edge, which should be considered as the
   * negative log-likelihood of the input path taken to reach the node.
   */
  get inputSamplingCost(): number {
    return this.matchedResult.tokenCorrections.reduce((accum, curr) => accum + curr.inputSamplingCost, 0);
  }

  /**
   * Gets the "total cost" of the edge, which should be considered as the
   * negative log-likelihood of the input path taken to reach the node
   * multiplied by the 'probability' induced by needed Damerau-Levenshtein edits
   * to the resulting output.
   */
  get totalCost(): number {
    return this.matchedResult.tokenCorrections.reduce((total, curr) => total + curr.totalCost, 0);
  }
}