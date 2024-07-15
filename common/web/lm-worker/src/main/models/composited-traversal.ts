export class CompositedTraversal implements LexiconTraversal {
  private readonly componentTraversals: Distribution<LexiconTraversal>;

  constructor(traversalsWithWeighting: Distribution<LexiconTraversal>) {
    let totalP = traversalsWithWeighting.reduce((accum, current) => accum + current.p, 0);
    this.componentTraversals = traversalsWithWeighting.map((entry) => {
      // Shallow-copy the traversals while normalizing the associated weightings.
      // The sum of all `p` values should sum to 1.
      return {
        sample: entry.sample,
        p: entry.p / totalP
      };
    });
  }

  // Eww... mixing these in generator-mode might be an 'issue'.  Esp if trying
  // to guarantee sorted order among them.
  children(): Generator<{ char: string; traversal: () => LexiconTraversal; }, any, unknown> {
    throw new Error("Method not implemented.");
  }

  child(char: string): LexiconTraversal {
    const children = this.componentTraversals.map((traversalTuple) => {
      return {
        sample: traversalTuple.sample.child(char),
        p: traversalTuple.p
      };
    });

    children.filter((entry) => !!entry.sample);

    if(children.length == 0) {
      return undefined;
    } else {
      return new CompositedTraversal(children);
    }
  }

  get entries(): TextWithProbability[] {
    // TODO:  Optimization?
    // - if each is already sorted, just... zipper-merge the two arrays.
    //   - Why O(N log N) when we can just O(N)?
    //   - granted, it's not like we expect large arrays here.
    const results = this.componentTraversals.flatMap((traversalTuple) => {
      return traversalTuple.sample.entries.map((entry) => {
        const reweightedEntry: TextWithProbability = {
          text: entry.text,
          p: entry.p * traversalTuple.p
        };

        return reweightedEntry;
      });
    });

    results.sort((a, b) => b.p - a.p);

    return results;
  }

  get p(): number {
    return this.componentTraversals
      // to 'traversal weighting' * 'traversal node's max probability'
      .map((entry) => entry.p * entry.sample.p)
      // find the best entry post-weighting
      .reduce((best, current) => (current > best) ? current : best, 0);
  }
}