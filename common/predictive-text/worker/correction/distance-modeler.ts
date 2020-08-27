/// <reference path="classical-calculation.ts" />

namespace correction {
  enum SearchOperation {
    addInput,
    addMatch,
    expandDiagonal
  }

  type RealizedInput = ProbabilityMass<Transform>[];

  type TraversableToken<TUnit> = {
    key: TUnit,
    traversal: LexiconTraversal
  }

  export const QUEUE_EDGE_COMPARATOR: models.Comparator<SearchEdge> = function(arg1, arg2) {
    return arg1.currentCost - arg2.currentCost;
  }

  // const QUEUE_END_NODE_COMPARATOR: models.Comparator<SearchNode> = function(arg1, arg2) {
  //   return arg1.knownCost - arg2.knownCost;
  // }

  export const QUEUE_SPACE_COMPARATOR: models.Comparator<SearchSpaceTier> = function(space1, space2) {
    let node1 = space1.correctionQueue.peek();
    let node2 = space2.correctionQueue.peek();
    
    // Guards, just in case one of the search spaces ever has an empty node.
    if(node1 && node2) {
      return node1.currentCost - node2.currentCost;
    } else if(node2) {
      return 1;
    } else {
      return -1;
    }
  }

  // Represents an 'edge' to a potential 'node' on the (conceptual) graph used to search for best-fitting
  // corrections by the correction-search algorithm.  Stores the cost leading to the new node, though it may be
  // an overestimate when the edit distance is greater than the current search threshold.
  //
  // For nodes with raw edit-distance cost within the current threshold for correction searches, we do have admissibility.
  // If not enough nodes are available within that threshold, however, admissibility may be lost, leaving our search as a
  // heuristic.
  class SearchEdge {
    // Existing calculation from prior rounds to use as source.
    calculation: ClassicalDistanceCalculation<string, EditToken<string>, TraversableToken<string>>;

    // The sequence of input 'samples' taken from specified input distributions.
    optimalInput: RealizedInput;

    // Returns the new input character + probabilty component to add in the current search space's
    // calculation layer.
    get currentInput(): ProbabilityMass<Transform> {
      if(!Array.isArray(this.optimalInput)) {
        return undefined;
      }

      let length = this.optimalInput.length;
      return this.optimalInput[length-1];
    }
    
    get knownCost(): number {
      return this.calculation.getHeuristicFinalCost();
    }

    get heuristicCost(): number {
      // TODO:  Optimize so that we're not frequently recomputing this?
      // TODO:  We might should generalize this so that the probability-to-cost function isn't directly hard-coded.
      //        Seems like a decent first conversion function though, at least.
      return this.optimalInput.map(mass => mass.p).reduce((previous, current) => previous + (1 - current), 0);
    }

    // The part used to prioritize our search.
    get currentCost(): number {
      return this.knownCost + this.heuristicCost;
    }

    get mapKey(): string {
      // TODO:  correct, as Transforms don't convert nicely to strings.
      let inputString = this.optimalInput.map((value) => value.sample).join('');
      let matchString =  this.calculation.matchSequence.map((value) => value.key).join('');
      // TODO:  might should also track diagonalWidth.
      return inputString + models.SENTINEL_CODE_UNIT + matchString;
    }
  }

  // Represents a processed node for the correction-search's search-space's tree-like graph.  May represent
  // internal and 'leaf' nodes on said graph, as well as the overall root of the search.
  //
  // Provides functions usable to enumerate across the node's outward edges to new nodes for continued search.
  // Most of the actual calculations occur as part of this process.
  //
  export class SearchNode {
    calculation: ClassicalDistanceCalculation<string, EditToken<string>, TraversableToken<string>>;
    
    currentTraversal: LexiconTraversal;
    priorInput: RealizedInput;

    // TODO:  Initializing from 'root' / just a traversal.
    /**
     * Instantiates the initial SearchNode used for corrective edit-distance based search.
     * 
     * @param traversal The root LexiconTraversal of a LexicalModel
     */
    constructor(traversal: LexiconTraversal);
    /**
     * Transforms an edge on the search graph into a node, utilizing its information
     * to determine the graph edges that may come afterward.
     * @param edge An existing SearchEdge used to to reach this SourceNode on the search algorithm's
     * 'graph'.
     */
    constructor(edge: SearchEdge);
    constructor(obj: SearchEdge|LexiconTraversal) {
      if(obj instanceof SearchEdge) {
        let edge = obj as SearchEdge;

        this.calculation = edge.calculation;
        this.currentTraversal = edge.calculation.matchSequence[edge.calculation.matchSequence.length-1].traversal;
        this.priorInput = edge.optimalInput;
      } else {
        // Assume it's a LexiconTraversal instead.
        let traversal = obj as LexiconTraversal;
        this.calculation = new ClassicalDistanceCalculation();
        this.currentTraversal = traversal;
        this.priorInput = [];
      }
    }

    buildInsertionEdges(): SearchEdge[] {
      let edges: SearchEdge[] = [];

      for(let lexicalChild of this.currentTraversal.children()) {
        let matchToken = {
          key: lexicalChild.char,
          traversal: lexicalChild.traversal()
        }

        // TODO:  Check against cache(s) & cache results.
        let childCalc = this.calculation.addMatchChar(matchToken);

        let searchChild = new SearchEdge();
        searchChild.calculation = childCalc;
        searchChild.optimalInput = this.priorInput;

        edges.push(searchChild);
      }

      return edges;
    }

    buildDeletionEdges(inputDistribution: ProbabilityMass<Transform>[]): SearchEdge[] {
      let edges: SearchEdge[] = [];

      for(let probMass of inputDistribution) {
        let edgeCalc = this.calculation;
        let transform = probMass.sample;
        if(transform.deleteLeft) {
          edgeCalc = edgeCalc.getSubset(edgeCalc.inputSequence.length - transform.deleteLeft, edgeCalc.matchSequence.length);
        }

        // TODO:  transform.deleteRight currently not supported.

        let inputPath = Array.from(this.priorInput);
        inputPath.push(probMass);
        // Tokenize and iterate over input chars, adding them into the calc.
        for(let i=0; i < transform.insert.length; i++) {
          let char = transform.insert[i];
          if(models.isHighSurrogate(char)) {
            i++;
            char = char + transform.insert[i];
          }

          // TODO:  Check against cache, write results to that cache.
          edgeCalc = edgeCalc.addInputChar({key: char});
        }

        let childEdge = new SearchEdge();
        childEdge.calculation = edgeCalc;
        childEdge.optimalInput = inputPath;

        edges.push(childEdge);
      }

      return edges;
    }

    // While this may SEEM to be unnecessary, note that sometimes substitutions (which are computed
    // via insert + delete) may be lower cost than both just-insert and just-delete.
    buildSubstitutionEdges(inputDistribution: ProbabilityMass<Transform>[]): SearchEdge[] {
      // Handles the 'input' component.
      let intermediateEdges = this.buildDeletionEdges(inputDistribution);
      let edges: SearchEdge[] = [];

      for(let lexicalChild of this.currentTraversal.children()) {
        for(let edge of intermediateEdges) {
          let matchToken = {
            key: lexicalChild.char,
            traversal: lexicalChild.traversal()
          }
  
          // TODO:  Check against cache(s), cache results.
          let childCalc = edge.calculation.addMatchChar(matchToken);
  
          let searchChild = new SearchEdge();
          searchChild.calculation = childCalc;
          searchChild.optimalInput = edge.optimalInput;
  
          edges.push(searchChild);
        }
      }

      return edges;
    }
  } 

  /*
   * NOTE:  Everything after this point is EXTREMELY rough-draft. 
   */

  class SearchSpaceTier {
    correctionQueue: models.PriorityQueue<SearchEdge>;
    operation: SearchOperation;

    processed: SearchNode[] = [];

    constructor(operation: SearchOperation) {
      this.operation = operation;
      this.correctionQueue = new models.PriorityQueue<SearchEdge>(QUEUE_EDGE_COMPARATOR);
    }
  }

  // The set of search spaces corresponding to the same 'context' for search.
  // Whenever a wordbreak boundary is crossed, a new instance should be made.
  export class SearchSpace {
    private cachedSpaces: {[id: string]: SearchSpaceTier} = {};
    private selectionQueue: models.PriorityQueue<SearchSpaceTier>;

    // TODO:  Fix; is not quite right.  We want the results corresponding to the node
    // that will let us build the next tier's SearchNodes when new input arrives.
    //
    // We use an array and not a PriorityQueue b/c batch-heapifying at a single point in time 
    // is cheaper than iteratively building a priority queue.
    private extractedResults: SearchNode[] = [];

    constructor() {
      this.selectionQueue = new models.PriorityQueue<SearchSpaceTier>(QUEUE_SPACE_COMPARATOR);
    }

    processNode(node: SearchEdge): SearchNode[] {
      let sourceCalc = node.calculation;

      // TODO:  Lots of things.
      return [];
    }

    addInput(input: Distribution<Transform>) {

    }
  }

  export class DistanceModelerOptions {
    minimumPredictions: number;
  }

  export class DistanceModeler {
    private options: DistanceModelerOptions;
    public static readonly DEFAULT_OPTIONS: DistanceModelerOptions = {
      minimumPredictions: 3
    }

    // Keep as a 'rotating cache'.  Includes search spaces corresponding to 'revert' commands.
    private searchSpaces: SearchSpace[] = [];

    private inputs: Distribution<USVString>[] = [];
    private lexiconRoot: LexiconTraversal;

    constructor(lexiconRoot: LexiconTraversal, options: DistanceModelerOptions = DistanceModeler.DEFAULT_OPTIONS) {
      this.lexiconRoot = lexiconRoot;
      this.options = options;
    }

    addInput(input: Distribution<Transform>) {
      // TODO:  add 'addInput' operation
      //        do search space things
    }

    // Current best guesstimate of how compositor will retrieve ideal corrections.
    getBestMatches(): Generator<[string, number][]> { // might should also include a 'base cost' parameter of sorts?
      // Duplicates underlying Priority Queue, iterates progressively through sets of evenly-costed
      // corrections until satisfied.
      return null;
    }
  }
}