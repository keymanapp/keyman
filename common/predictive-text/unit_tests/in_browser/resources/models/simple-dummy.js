/**
 * While handwritten, this class is designed to mirror the potential results of TypeScript compilation of
 * model TS source files.
 */
(function(){
  var Model = /** @class */ (function() {
    function Model() { // implements Model
    }

    Model.punctuation = {
      quotesForKeepSuggestion: { open: '“', close: '”'},
      // Important! Set this, or else the model compositor will
      // insert something for us!
      insertAfterWord: "",
    };

    // A direct import/copy from i_got_distracted_by_hazel.json.
    Model.futureSuggestions = [
      [
        {
          "transform": {
            "insert": "I ",
            "deleteLeft": 0
          },
          "displayAs": "I"
        },
        {
          "transform": {
            "insert": "I'm ",
            "deleteLeft": 0
          },
          "displayAs": "I'm"
        },
        {
          "transform": {
            "insert": "Oh ",
            "deleteLeft": 0
          },
          "displayAs": "Oh"
        }
      ],
      [
        {
          "transform": {
            "insert": "love ",
            "deleteLeft": 0
          },
          "displayAs": "love"
        },
        {
          "transform": {
            "insert": "am ",
            "deleteLeft": 0
          },
          "displayAs": "am"
        },
        {
          "transform": {
            "insert": "got ",
            "deleteLeft": 0
          },
          "displayAs": "got"
        }
      ],
      [
        {
          "transform": {
            "insert": "distracted ",
            "deleteLeft": 0
          },
          "displayAs": "distracted by"
        },
        {
          "transform": {
            "insert": "distracted ",
            "deleteLeft": 0
          },
          "displayAs": "distracted"
        },
        {
          "transform": {
            "insert": "a ",
            "deleteLeft": 0
          },
          "displayAs": "a"
        }
      ],
      [
        {
          "transform": {
            "insert": "Hazel ",
            "deleteLeft": 0
          },
          "displayAs": "Hazel"
        },
        {
          "transform": {
            "insert": "the ",
            "deleteLeft": 0
          },
          "displayAs": "the"
        },
        {
          "transform": {
            "insert": "a ",
            "deleteLeft": 0
          },
          "displayAs": "a"
        }
      ]
    ];    

    return Model;
  }());

  // It's a 'dummy' model, so there's no need for extra methods and such within the Model's class definition.
  LMLayerWorker.loadModel(new models.DummyModel({futureSuggestions: Model.futureSuggestions, punctuation: Model.punctuation}));
})();