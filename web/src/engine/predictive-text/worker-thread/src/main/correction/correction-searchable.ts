/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2026-04-02
 *
 * This file defines the interface required of objects that represent portions
 * of a search graph or search quotient-graph compatible with the
 * `getBestMatches` algorithm.
 */

import { CorrectionResultMapping } from "./correction-result-mapping.js";

type NullPath = {
  type: 'none'
}

type IntermediateSearchPath = {
  type: 'intermediate',
  cost: number
}

type CompleteSearchPath<MappingType> = {
  type: 'complete',
  cost: number,
  mapping: MappingType
}

export type PathResult<MappingType> = NullPath | IntermediateSearchPath | CompleteSearchPath<MappingType>;

/**
 * Represents objects that support correction search via the `getBestMatches`
 * method, providing metadata relative to optimizing the search process for
 * their represented portion of the search-graph.
 */
export interface CorrectionSearchable<ResultType, ResultMapping extends CorrectionResultMapping<ResultType>> {
  /**
   * The best cost found for any search paths yet unprocessed by either this "searchable" or any of its ancestors.
   */
  readonly currentCost: number;

  /**
   * A list of all search path results already found that terminate at this "searchable".
   */
  readonly previousResults: ResultMapping[];

  /**
   * Processes the most likely currently-unprocessed search path represented by this "searchable".
   */
  handleNextNode(): PathResult<ResultMapping>;
}