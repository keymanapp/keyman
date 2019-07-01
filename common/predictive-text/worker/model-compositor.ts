class ModelCompositor {
  private lexicalModel: WorkerInternalModel;
  private static readonly MAX_SUGGESTIONS = 12;

  constructor(lexicalModel: WorkerInternalModel) {
    this.lexicalModel = lexicalModel;
  }

  predict(transformDistribution: Transform | Distribution<Transform>, context: Context): Suggestion[] {
    let suggestionDistribution: Distribution<Suggestion> = [];

    // Assumption:  Duplicated 'displayAs' properties indicate duplicated Suggestions.
    // When true, we can use an associated array to de-duplicate everything.
    let suggestionDistribMap: {[key: string]: ProbabilityMass<Suggestion>} = {};

    if(!(transformDistribution instanceof Array)) {
      transformDistribution = [ {sample: transformDistribution, p: 1.0} ];
    }

    // Find the transform for the actual keypress.
    let inputTransform = transformDistribution.sort(function(a, b) {
      return b.p - a.p;
    })[0].sample;

    // Only allow new-word suggestions if space was the most likely keypress.
    let allowSpace = inputTransform.insert == " " || inputTransform.insert == "\n";
    let allowBksp = inputTransform.insert == "" && inputTransform.deleteLeft > 0;

    let postContext = models.Common.applyTransform(inputTransform, context);
    let keepOptionText = this.lexicalModel.wordbreak(postContext);
    let keepOption: Suggestion = null;

    for(let alt of transformDistribution) {
      let transform = alt.sample;

      // Filter out special keys unless they're expected.
      if((transform.insert == " " || transform.insert == "\n") && !allowSpace) {
        continue;
      } else if(transform.insert == "" && transform.deleteLeft > 0 && !allowBksp) {
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
          keepOption.isKeep = true;
        } else {
          let s = suggestionDistribMap[displayText];
          if(s) {
            s.p += pair.p * alt.p;
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
        isKeep: true
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
      if(suggestions.length == ModelCompositor.MAX_SUGGESTIONS) {
        suggestions.pop();
      }

      suggestions = [ keepOption ].concat(suggestions);
    }

    return suggestions;
  }
}