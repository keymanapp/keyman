/**
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2026-03-10
 *
 * This file adds helper functions useful for developing test assertions against
 * different types of SearchQuotientNodes and their properties.
 */

import {
  DeletionQuotientSpur,
  InsertionQuotientSpur,
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
    const parentPaths = constituentPaths(node.parents[0]);    let pathsToExtend = parentPaths;

    if(node instanceof InsertionQuotientSpur) {
      pathsToExtend = pathsToExtend.filter(s => {
        const tail = s[s.length - 1];

        // Deletion nodes and modules should always be ordered after those for
        // insertion in order to avoid duplicating search paths.  (Insertions may
        // stick to the right of a root, while deletions always process inputs; they
        // may thus precede deletions.)
        //
        // Also, internally, insertion edges are not built after deletion (or empty) edges.
        if(tail instanceof DeletionQuotientSpur) {
          return false;
        } else if(tail.insertLength == 0 && tail.leftDeleteLength == 0) {
          // Insertions should also not appear after empty nodes; there's no net
          // difference between inserting before and inserting after.
          return false;
        }

        return true;
      });
    }

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