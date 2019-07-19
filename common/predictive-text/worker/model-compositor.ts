/**
 * This class represents a potential keystroke's transform that may function as part of our
 * language-modeling text source context, probabilistically modeling adjustments to
 * the observed output needed to obtained desired predictive or corrective outputs.
 */
class ContextTransform {
  /**
   * The root `Transform` on which any 'alternate' `Transform`s contained by this object are based.
   */
  original: ProbabilityMass<Transform>;

  /**
   * The 'compiled' result of applyContext applied with `original.sample` and the existing context 
   * from the previous entry in the sequence.
   */
  result: Context; 

  /**
   * Potential alternate `Transform`s that may have been intended in place of our root 'original'.
   * 
   * This field should only be populated for the **root** `ContextTransform` in the tree, as replacements
   * would otherwise attempt to replace each other needlessly.
   * 
   * Deletion of the root may be considered via an 'null replacement' `ContextTransform` with no children:
   * null-valued (no insert, no delete) `Transform` with the probability of omitting a `Transform `here entirely.
   */
  replacements: ContextTransform[];
  
  /**
   * Potential `Transform`s not observed that may have been intended for insertion _after_ this
   * `ContextTransform` but _before_ the next one in the sequence.
   * 
   * This field is orthogonal to `replacements` - each `replacement` may instead specify its own 
   * `insertions` values.
   * 
   * While adding depth to the tree enhances our accuracy, it will drop our efficiency and performance.
   * We'll want to apply some strong limits and/or heuristics here.
   */
  insertions: ContextTransform[];
}

/**
 * A collection of `ContextTransforms` modeling potential user-intended variations of our observed `Context`
 * in a keyboard-aware manner corresponding to a modeled word.
 * 
 */
class TransformContext {
  /**
   * Models the sequence of inputs that produced the current word and potential variations thereof, rooted at each index.
   * 
   * Index 0 should contain a 'null-valued' root `Transform` with no replacements to allow modeling word-initial
   * insertions of new keystrokes.
   * 
   * Note that each `ContextTransform`'s variations are computed independently of each other; it is possible the resulting
   * prediction for the `Context` may not actually match the output from the keyboard that would match those keystrokes when
   * complex rules may match the resulting `Context`.
   */
  sequence: ContextTransform[];
}

class ModelCompositor {
  private lexicalModel: WorkerInternalModel;
  private static readonly MAX_SUGGESTIONS = 12;

  constructor(lexicalModel: WorkerInternalModel) {
    this.lexicalModel = lexicalModel;
  }

  protected explodeString(str: string): string[] {
    let exploded = str.split('');

    for(let i=0; i < exploded.length; i++) {
      // Check for surrogate pairs
      let code = exploded[i].charCodeAt(0);;
      if(code >= 0xD800 && code <= 0xDBFF && (i + 1) < exploded.length) {
        let pairedCode = exploded[i+1].charCodeAt(i);

        // Ensure it's actually a legit paired code
        if(pairedCode >= 0xDC00 && pairedCode <= 0xDFFF) {
          // It's SMP - merge the two!
          exploded.splice(i, 2, exploded[i] + exploded[i+1]);
        }
      }
    }

    return exploded;
  }

  protected generateTransformContext(context: Context): TransformContext {
    let charSet = this.lexicalModel.characterSet;
    if(!charSet) {
      return null;
    }

    let chars = charSet.map(function(pair) {
      return pair[0];
    });

    let uniformCharDistribution: Distribution<Transform> = charSet.map(function(pair) {
      return { sample: {insert: pair[0], deleteLeft: 0}, p: pair[1]/charSet.length };
    });

    // #1 - use split the context's current word into recognized chars, making sure to be SMP-aware as we do so.
    let currentWord = this.lexicalModel.wordbreak(context);
    let explodedWord = this.explodeString(currentWord);

    let extendedContext = new TransformContext();
    extendedContext.sequence = [];

    for(let i=0; i < explodedWord.length; i++) {
      let contextTransform = new ContextTransform();
      // Creates a basic insert-only Transform based on the raw text.
      contextTransform.original = {sample: {insert: explodedWord[i], deleteLeft: 0}, p: 1};

      extendedContext.sequence.push(contextTransform);
    }

    // Now that the word is spliced, we have the 'originals' for the TransformContext sequence.
    // We can build the base ContextTransform objects, but what's the best way to set up the subtrees?
 
    // TODO:  Return something actually-useful instead.
    return null;
  }

  protected isWhitespace(transform: Transform): boolean {
    // Matches prefixed text + any instance of a character with Unicode general property Z* or the following: CR, LF, and Tab.
    // TODO:  Unfortunately, this regex isn't IE-compatible.  Find a solution that at least
    //        doesn't prevent loading on IE.
    //let whitespaceRemover = /.*[\u0009\u000A\u000D\u0020\u00a0\u1680\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200a\u2028\u2029\u202f\u205f\u3000]/iu;
    
    // Temp solution:
    let whitespaceRemover = /.*\s/; // At least handles standard whitespace.
    let insert = transform.insert;

    insert = insert.replace(whitespaceRemover, '');

    return insert == '';
  }

  protected isBackspace(transform: Transform): boolean {
    return transform.insert == "" && transform.deleteLeft > 0;
  }

  public correct(transformDistribution: Transform | Distribution<Transform>, context: Context): Suggestion[] {
    let suggestionDistribution: Distribution<Suggestion> = [];

    if(!(transformDistribution instanceof Array)) {
      transformDistribution = [ {sample: transformDistribution, p: 1.0} ];
    }

    // Step 1:  determine the existing context and the transforms that got us here.
    let currentPrefix = this.lexicalModel.wordbreak(context);
    // TODO:  Build transform sequence.  Current type plan:  TransformContext

    // Step 2:  determine the new transforms that can reasonably apply.
    // TODO:  Build transform sequence variants



    // Find the transform for the actual keypress.
    let inputTransform = transformDistribution.sort(function(a, b) {
      return b.p - a.p;
    })[0].sample;

    // Assumption:  Duplicated 'displayAs' properties indicate duplicated Suggestions.
    // When true, we can use an 'associative array' to de-duplicate everything.
    let suggestionDistribMap: {[key: string]: ProbabilityMass<Suggestion>} = {};

    // Only allow new-word suggestions if space was the most likely keypress.
    let allowSpace = this.isWhitespace(inputTransform);
    let allowBksp = this.isBackspace(inputTransform);

    let postContext = models.applyTransform(inputTransform, context);
    let keepOptionText = this.lexicalModel.wordbreak(postContext);
    let keepOption: Suggestion = null;

    // if(this.lexicalModel.characterSet) {
    // //   // We can attempt use of edit distance calculations.
    // }

    for(let alt of transformDistribution) {
      let transform = alt.sample;

      // Filter out special keys unless they're expected.
      if(this.isWhitespace(transform) && !allowSpace) {
        continue;
      } else if(this.isBackspace(transform) && !allowBksp) {
        continue;
      }
      let distribution = this.lexicalModel.predict(transform, context);

      distribution.forEach(function(pair: ProbabilityMass<Suggestion>) {
        // Let's not rely on the model to copy transform IDs.
        // Only bother is there IS an ID to copy.
        if(transform.id !== undefined) {
          pair.sample.transformId = transform.id;
        }

        // Combine duplicate samples.
        let displayText = pair.sample.displayAs;

        if(displayText == keepOptionText) {
          keepOption = pair.sample;
          // Specifying 'keep' helps uses of the LMLayer find it quickly
          // if/when desired.
          keepOption.tag = 'keep';
        } else {
          let existingSuggestion = suggestionDistribMap[displayText];
          if(existingSuggestion) {
            existingSuggestion.p += pair.p * alt.p;
          } else {
            let compositedPair = {sample: pair.sample, p: pair.p * alt.p};
            suggestionDistribMap[displayText] = compositedPair;
          }
        }
      });
    }

    // Generate a default 'keep' option if one was not otherwise produced.
    if(!keepOption && keepOptionText != '') {
      keepOption = {
        displayAs: keepOptionText,
        transformId: inputTransform.id,
        // Replicate the original transform, modified for appropriate language insertion syntax.
        transform: {
          insert: inputTransform.insert + ' ',
          deleteLeft: inputTransform.deleteLeft,
          deleteRight: inputTransform.deleteRight,
          id: inputTransform.id
        },
        tag: 'keep'
      };
    }

    // Now that we've calculated a unique set of probability masses, time to make them into a proper
    // distribution and prep for return.
    for(let key in suggestionDistribMap) {
      let pair = suggestionDistribMap[key];
      suggestionDistribution.push(pair);
    }

    suggestionDistribution = suggestionDistribution.sort(function(a, b) {
      return b.p - a.p; // Use descending order - we want the largest probabilty suggestions first!
    });

    let suggestions = suggestionDistribution.splice(0, ModelCompositor.MAX_SUGGESTIONS).map(function(value) {
      return value.sample;
    });

    if(keepOption) {
      suggestions = [ keepOption ].concat(suggestions);
    }

    return suggestions;
  }

  public predict(transformDistribution: Transform | Distribution<Transform>, context: Context): Suggestion[] {
    let suggestionDistribution: Distribution<Suggestion> = [];

    // Assumption:  Duplicated 'displayAs' properties indicate duplicated Suggestions.
    // When true, we can use an 'associative array' to de-duplicate everything.
    let suggestionDistribMap: {[key: string]: ProbabilityMass<Suggestion>} = {};

    if(!(transformDistribution instanceof Array)) {
      transformDistribution = [ {sample: transformDistribution, p: 1.0} ];
    }

    // Find the transform for the actual keypress.
    let inputTransform = transformDistribution.sort(function(a, b) {
      return b.p - a.p;
    })[0].sample;

    // Only allow new-word suggestions if space was the most likely keypress.
    let allowSpace = this.isWhitespace(inputTransform);
    let allowBksp = this.isBackspace(inputTransform);

    let postContext = models.applyTransform(inputTransform, context);
    let keepOptionText = this.lexicalModel.wordbreak(postContext);
    let keepOption: Suggestion = null;

    for(let alt of transformDistribution) {
      let transform = alt.sample;

      // Filter out special keys unless they're expected.
      if(this.isWhitespace(transform) && !allowSpace) {
        continue;
      } else if(this.isBackspace(transform) && !allowBksp) {
        continue;
      }
      let distribution = this.lexicalModel.predict(transform, context);

      distribution.forEach(function(pair: ProbabilityMass<Suggestion>) {
        // Let's not rely on the model to copy transform IDs.
        // Only bother is there IS an ID to copy.
        if(transform.id !== undefined) {
          pair.sample.transformId = transform.id;
        }

        // Combine duplicate samples.
        let displayText = pair.sample.displayAs;

        if(displayText == keepOptionText) {
          keepOption = pair.sample;
          // Specifying 'keep' helps uses of the LMLayer find it quickly
          // if/when desired.
          keepOption.tag = 'keep';
        } else {
          let existingSuggestion = suggestionDistribMap[displayText];
          if(existingSuggestion) {
            existingSuggestion.p += pair.p * alt.p;
          } else {
            let compositedPair = {sample: pair.sample, p: pair.p * alt.p};
            suggestionDistribMap[displayText] = compositedPair;
          }
        }
      });
    }

    // Generate a default 'keep' option if one was not otherwise produced.
    if(!keepOption && keepOptionText != '') {
      keepOption = {
        displayAs: keepOptionText,
        transformId: inputTransform.id,
        // Replicate the original transform, modified for appropriate language insertion syntax.
        transform: {
          insert: inputTransform.insert + ' ',
          deleteLeft: inputTransform.deleteLeft,
          deleteRight: inputTransform.deleteRight,
          id: inputTransform.id
        },
        tag: 'keep'
      };
    }

    // Now that we've calculated a unique set of probability masses, time to make them into a proper
    // distribution and prep for return.
    for(let key in suggestionDistribMap) {
      let pair = suggestionDistribMap[key];
      suggestionDistribution.push(pair);
    }

    suggestionDistribution = suggestionDistribution.sort(function(a, b) {
      return b.p - a.p; // Use descending order - we want the largest probabilty suggestions first!
    });

    let suggestions = suggestionDistribution.splice(0, ModelCompositor.MAX_SUGGESTIONS).map(function(value) {
      return value.sample;
    });

    if(keepOption) {
      suggestions = [ keepOption ].concat(suggestions);
    }

    return suggestions;
  }

  private buildTransformContext(context: Context) {
    // Step 1:  determine the existing context and the transforms that got us here.
    let currentPrefix = this.lexicalModel.wordbreak(context);
  }

}