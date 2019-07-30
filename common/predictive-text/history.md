# KeymanWeb Version History

## 2019-07-30 12.0.0 beta
* Started development of the LMLayer interface for common work on predictive modeling.
* Implemented WebWorker background thread for the heavy lifting; all API calls with relevant
  return values return Promises.
* Provides API for the following language-modeling functions:
  * predict - Prediction of user's desired word based on context and recent input
    * Also returns a tagged prediction corresponding to the original context state.
    * Corrections are currently limited to the most recent input keystroke.
    * Accepts a probability distribution for the most recent keystroke, which is
      be used to weight prediction probabilities
  * wordbreak - Performs wordbreaking on the current context state
    * A default wordbreaker based on the Unicode specification is provided as a default.
* Implements a common "template" for weighted wordlist lexical model functionality:
  * Accepts a "search term to key" function to facilitate common corrections, such as diacritics,
    during prediction lookups.
  * Predictions are weighted based on their frequency and their input likelihood (based on
    keystroke probabilities).
* Accepts fully-specified custom lexical models that do not rely on model "templates."