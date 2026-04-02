import { CorrectionSearchable } from "./correction-searchable.js";

export interface CorrectionResultMapping<ResultType> {
  readonly matchingSpace: CorrectionSearchable<ResultType, CorrectionResultMapping<ResultType>>;
  readonly matchedResult: ResultType;

  /**
   * Gets the "total cost" of the edge, which should be considered as the
   * negative log-likelihood of the input path taken to reach the node
   * multiplied by the 'probability' induced by needed Damerau-Levenshtein edits
   * to the resulting output.
   */
  readonly totalCost: number;
}