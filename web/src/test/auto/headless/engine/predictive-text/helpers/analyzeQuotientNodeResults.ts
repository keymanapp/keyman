/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2026-03-11
 *
 * This file defines a unit-text helper designed to test what search-graph
 * results for corrections may result from paths traversing through a given
 * SearchQuotientNode.
 */

import { PathResult, SearchQuotientNode } from "@keymanapp/lm-worker/test-index";

/**
 * Represents the results of a call to `analyzeQuotientNodeResults` during unit
 * testing.
 *
 * All specified `expectedResults` entries will be found once - either in
 * `found` or `missing` for each.  If `found`, the result _may_ also appear in
 * `foundWithDuplicates` if it is found more than once during the search.
 */
export interface SearchResultAnalysis {
  /**
   * Indicates specified strings (expected results) that were found via search
   * on the provided node one or more times.
   */
  found: string[];

  /**
   * Indicates specified strings (expected results) that were not found via
   * search on the provided node.
   */
  missing: string[];

  /**
   * Indicates specified strings (expected results) that were found via search
   * on the provided node more than once.
   */
  foundWithDuplicates: string[];
}

/**
 * Processes all correction-search search targets that have a path passing
 * through the specified SearchQuotientNode in order to determine whether or not
 * valid paths exist for specified ("expected") correction strings.
 *
 * @param node The SearchQuotientNode to utilize for the search.
 * @param expectedResults A set of strings used for the test scenario.  Strings
 * not found here will be ignored.
 * @returns
 */
export function analyzeQuotientNodeResults(
  node: SearchQuotientNode,
  expectedResults: string[]
) {
  const matchMap: Map<string, number> = new Map();

  let result: PathResult = node.handleNextNode();
  while(result.type != 'none') {
    if(result.type == 'complete') {
      const resultKey = result.mapping.matchString;
      if(expectedResults.find((entry) => entry == resultKey)) {
        matchMap.set(resultKey, (matchMap.get(resultKey) ?? 0) + 1);
      }
    }

    result = node.handleNextNode();
  }

  const analysis: SearchResultAnalysis = {
    found: [],
    missing: [],
    foundWithDuplicates: []
  };

  for(let key of expectedResults) {
    const matchCount = matchMap.get(key) ?? 0;

    if(matchCount == 0) {
      analysis.missing.push(key);
    } else {
      analysis.found.push(key);
      if(matchCount > 1) {
        analysis.foundWithDuplicates.push(key);
      }
    }
  }

  return analysis;
}