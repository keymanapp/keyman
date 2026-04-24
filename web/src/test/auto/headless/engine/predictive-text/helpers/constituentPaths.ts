/**
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2026-03-10
 *
 * This file adds helper functions useful for developing test assertions against
 * different types of SearchQuotientNodes and their properties.
 */

import {
  SearchQuotientCluster,
  SearchQuotientNode,
  SearchQuotientRoot,
  SearchQuotientSpur
} from "@keymanapp/lm-worker/test-index";

/**
 * Enumerates the different potential SearchQuotientSpur sequences that lead
 * to a SearchQuotientNode.
 *
 * Intended only for use during unit testing.  Does not include the root node.
 */
export function constituentPaths(node: SearchQuotientNode): SearchQuotientSpur[][] {
  if(node instanceof SearchQuotientRoot) {
    return [];
  } else if(node instanceof SearchQuotientCluster) {
    return node.parents.flatMap((p) => constituentPaths(p));
  } else if(node instanceof SearchQuotientSpur) {
    const parentPaths = constituentPaths(node.parents[0]);
    let pathsToExtend = parentPaths;

    if(parentPaths.length > 0) {
      return pathsToExtend.map(p => {
        p.push(node);
        return p;
      });
    } else {
      return [[node]];
    }
  } else {
    throw new Error("constituentPaths is unable to handle a new, unexpected SearchQuotientNode type");
  }
}