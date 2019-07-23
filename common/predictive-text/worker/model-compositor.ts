class ModelCompositor {
  private lexicalModel: WorkerInternalModel;
  private static readonly MAX_SUGGESTIONS = 12;

  constructor(lexicalModel: WorkerInternalModel) {
    this.lexicalModel = lexicalModel;
  }

  protected isWhitespace(transform: Transform): boolean {
    // Matches prefixed text + any instance of a character with Unicode general property Z* or the following: CR, LF, and Tab.
    let whitespaceRemover = /.*[\u0009\u000A\u000D\u0020\u00a0\u1680\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200a\u200b\u2028\u2029\u202f\u205f\u3000]/i;
    
    // Filter out null-inserts; their high probability can cause issues.
    if(transform.insert == '') { // Can actually register as 'whitespace'.
      return false;
    }

    let insert = transform.insert;

    insert = insert.replace(whitespaceRemover, '');

    return insert == '';
  }

  protected isBackspace(transform: Transform): boolean {
    return transform.insert == "" && transform.deleteLeft > 0;
  }

  predict(transformDistribution: Transform | Distribution<Transform>, context: Context): Suggestion[] {
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

      let preserveWhitespace: boolean = false;
      if(this.isWhitespace(transform)) {
        // Detect start of new word; prevent whitespace loss here.
        let postContext = models.applyTransform(transform, context);
        preserveWhitespace = (this.lexicalModel.wordbreak(postContext) == '');
      }

      let distribution = this.lexicalModel.predict(transform, context);

      distribution.forEach(function(pair: ProbabilityMass<Suggestion>) {
        // Let's not rely on the model to copy transform IDs.
        // Only bother is there IS an ID to copy.
        if(transform.id !== undefined) {
          pair.sample.transformId = transform.id;
        }

        // Prepends the original whitespace, ensuring it is preserved if
        // the suggestion is accepted.
        if(preserveWhitespace) {
          models.prependTransform(pair.sample.transform, transform);
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

    // TODO:  Customizable modeling for this formatting.  Different languages
    //        use different quotation styles.  See https://github.com/keymanapp/keyman/issues/1883.
    if(keepOption) {
      keepOption.displayAs = '"' + keepOption.displayAs + '"';
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
}