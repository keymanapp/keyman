/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2026-04-02
 *
 * This file defines the type used for tracking critical graph-search properties
 * utilized during correction-search by any type compatible with the
 * `getBestMatches` algorithm.
 */

import { CorrectionSearchable } from "./correction-searchable.js";

/**
 * Any return value from `.handleNextNode()` designed for use with the
 * `getBestMatches` method must adhere to this type interface for representing
 * completed search paths.
 */
export interface CorrectionResultMapping<ResultType> {
  /**
   * Represents the "searchable" object through which the completed search path last traversed.
   */
  readonly matchingSpace: CorrectionSearchable<ResultType, CorrectionResultMapping<ResultType>>;

  /**
   * The object representing the search path completed at the current search step.
   */
  readonly matchedResult: Readonly<ResultType>;

  /**
   * Gets the "total cost" of the edge, which should be considered as the
   * negative log-likelihood of the input path taken to reach the node
   * multiplied by the 'probability' induced by needed Damerau-Levenshtein edits
   * to the resulting output.
   */
  readonly totalCost: number;
}