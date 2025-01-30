import { LexicalModelTypes } from "@keymanapp/common-types";
import Distribution = LexicalModelTypes.Distribution;
import LexiconTraversal = LexicalModelTypes.LexiconTraversal;
import TextWithProbability = LexicalModelTypes.TextWithProbability;
import { PriorityQueue } from "@keymanapp/web-utils";

type ChildEdge = { char: string; p: number, traversal: () => LexiconTraversal };

export class CompositedTraversal implements LexiconTraversal {
  private readonly componentTraversals: Distribution<LexiconTraversal>;

  // Used both internally and externally - and we don't want to re-weight internal calls!
  constructor(traversalsWithWeighting: Distribution<LexiconTraversal>) {
    // let totalP = traversalsWithWeighting.reduce((accum, current) => accum + current.p, 0);
    this.componentTraversals = traversalsWithWeighting.map((entry) => {
      // Shallow-copy the traversals while normalizing the associated weightings.
      // The sum of all `p` values should sum to 1.
      return {
        sample: entry.sample,
        p: entry.p // totalP
      };
    });
  }

  children(): ChildEdge[] {
    // Easy case:  when there's no source on the current Traversal path, there
    // are no children.
    //
    // Though... we shouldn't even reach this point; if a child path lacks
    // support; `undefined` should have been returned instead of a class
    // instance.

    /* c8 ignore start */
    if(this.componentTraversals.length == 0) {
      return [];
    }
    /* c8 ignore stop */

    // If there were no duplicated children, merging the arrays returned from the
    // following statement, as is, would be the correct answer.
    const unblendedComponents = this.componentTraversals.map((component) => {
      const weight = component.p;
      return {
        sample: component.sample.children().map((entry) => {
          return {
            char: entry.char,
            p: entry.p,
            traversal: () => entry.traversal()
          };
        }),
        // Make sure to track the model-source's weighting!
        weight: weight
      };
    });

    // If there's only a single active component, well, there actually are
    // no duplicates - thus it is the right answer.
    if(unblendedComponents.length == 1) {
      const singleSource = unblendedComponents[0];
      return singleSource.sample.map((child) => {
        return {
          char: child.char,
          p: singleSource.weight * child.p,
          traversal: () => new CompositedTraversal([{ sample: child.traversal(), p: singleSource.weight }])
        };
      });
    }

    // Now, the tricky part:  actual blending.

    // Find sets of traversals that traverse the same char path after the represented prefix.
    const componentBuckets: Record<string, { component: ChildEdge, weight: number }[]> = {};
    unblendedComponents.forEach((componentChildren) => {
      componentChildren.sample.forEach((entry) => {
        componentBuckets[entry.char] ??= [];
        componentBuckets[entry.char].push({
          component: entry,
          weight: componentChildren.weight
        });
      });
    });

    // For each char path...
    const keys = Object.keys(componentBuckets);
    return keys.map((key) => {
      const bucket = componentBuckets[key];
      // Build the blended traversal for that path
      const traversalFunc = () => {
        const traversalsWithWeighting = bucket.map((entry) => {
          return {
            sample: entry.component.traversal(),
            // Apply the model's weighting!
            // (This is not the `p` for that model's traversal path.)
            p: entry.weight
          };
        });
        return new CompositedTraversal(traversalsWithWeighting);
      };

      // If there's only one contributing model, there's no need to worry about duplicates
      // affecting the "highest probability" value for the path; no blending needed.
      //
      // It's thus safe to just return the model's probability for the path.
      if(bucket.length == 1) {
        return {
          char: key,
          p: bucket[0].weight * bucket[0].component.p,
          traversal: traversalFunc
        };
      } else {
        // Here, there's a non-zero chance that duplicate entries may exist.
        // We thus need to worry about the potential for blending...
        // which means we're best off delegating "highest probability" calculation
        // to the child, which is the level at which the issue may be resolved.
        const traversal = traversalFunc();

        return {
          char: key,
          // Use the traversal early to ensure duplicate entries aren't improperly
          // neglected due to their probability sources being split.
          p: traversal.p,
          // We've actually built it already, so just re-use the instance.
          traversal: () => traversal
        };
      }
    });
  }

  child(char: string): LexiconTraversal {
    let children = this.componentTraversals.map((traversalTuple) => {
      return {
        sample: traversalTuple.sample.child(char),
        p: traversalTuple.p
      };
    });

    children = children.filter((entry) => !!entry.sample);

    if(children.length == 0) {
      return undefined;
    } else {
      // Will sort out blending on its own.
      return new CompositedTraversal(children);
    }
  }

  get entries(): TextWithProbability[] {
    // If there were guaranteed to be no duplicates, .flatMap instead of .map would be fine here.
    const entrySets = this.componentTraversals.map((traversalTuple) => {
      return traversalTuple.sample.entries.map((entry) => {
        const reweightedEntry: TextWithProbability = {
          text: entry.text,
          p: entry.p * traversalTuple.p
        };

        return reweightedEntry;
      });
    });

    // Combine any duplicated entries.
    const resultMap: Record<string, TextWithProbability> = {};
    entrySets.forEach((set) => {
      set.forEach((entry) => {
        if(resultMap[entry.text]) {
          resultMap[entry.text].p += entry.p;
        } else {
          resultMap[entry.text] = entry;
        }
      });
    });

    // Finalize as array + sort by descending probability
    const results = Object.values(resultMap);
    results.sort((a, b) => b.p - a.p);

    return results;
  }

  get p(): number {
    // If it is impossible for a blended traversal path to yield a higher-
    // probability entry than the best unblended result, there's no point
    // in considering it here.
    const bestUnblendedProb = this.componentTraversals
      // to 'traversal weighting' * 'traversal node's max probability'
      .map((entry) => entry.p * entry.sample.p)
      // find the best entry post-weighting
      .reduce((best, current) => (current > best) ? current : best, 0);

    // Easy case:  1 component at this level?  No blending is possible,
    // so just return the best entry found this way.
    if(this.componentTraversals.length == 1) {
      return bestUnblendedProb;
    }

    // Check for traversal children that travel the same path.  O(sum of child
    // nodes across all components)
    //
    // If we wanted to try optimizing this...
    // - priority queue for each componentTraversal, descending prob order
    // - track which sources have contributed while accumulating path probs
    // - if best entry from sources not contributed + current accumulation can't
    //   win, we can stop blending early.
    //   - further iterations can no longer exceed bestUnblendedProb.
    //
    // This likely avoids iterating over all paths...  but invoking priority
    // queues means worst-case O(sum N_i log N_i), which is worse.  (With N_i =
    // path count per component traversal)
    // - constructable "worst case": fully balanced pMaxes?
    // - for "good cases"... would the complexity even be worth it?
    //   - suppose 3 sources + English:  worst-case iteration count with current
    //     setup is 4 * 26 = 104.  That's not "terrible"; fairly low to be
    //     considering big-O optimizations.
    //   - +1 multiplier due to the following `filter` block.
    const childBuckets: Record<string, { p?: number, pMax: number, char: string }> = {};
    const entryBuckets: Record<string, number> = {};
    this.componentTraversals.forEach((entry) => {
      const weight = entry.p;
      const component = entry.sample;
      const entries = component.entries;

      component.children().forEach((child) => {
        const record = childBuckets[child.char] ??= { pMax: 0, char: child.char };
        record.pMax += weight * child.p;
      });

      entries.forEach((entry) => entryBuckets[entry.text] = (entryBuckets[entry.text] ?? 0) + entry.p * weight);
    });

    // If a path's maximum potential probability does not exceed the best entry,
    // it can't affect our answer; skip it.
    // Is O(alphabet count)
    const potentialBlendedChildren = Object.keys(childBuckets).map((char) => {
      const record = childBuckets[char];
      if(record.pMax <= bestUnblendedProb) {
        return null;
      } else {
        return record;
      }
    }).filter((record) => !!record);

    let bestBlendedEntry = Object.values(entryBuckets).reduce((best, current) => Math.max(best, current), 0);

    // Prep the path-search for a potential winner.
    let bestProb = Math.max(bestUnblendedProb, bestBlendedEntry);
    // May have zero entries if no blended path can exceed the best non-blended path's probability.
    // (Assumption:  this is the common case.)
    const queue = new PriorityQueue((a, b) => b.pMax - a.pMax, potentialBlendedChildren);

    while(queue.peek()?.pMax > bestProb) {
      const entry = queue.dequeue();
      const childPath = this.child(entry.char);

      // Note:  this pattern, as-is, does not allow cross-node optimizations
      // (such as seeing a better potential here once we've started evaluating
      // potentials within the child)
      //
      // It's a reasonable "first pass" though.
      bestProb = Math.max(childPath.p, bestProb);
    }
    return bestProb;
  }
}