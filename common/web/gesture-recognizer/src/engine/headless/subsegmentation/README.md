## About this subfolder

This folder contains code for an advanced touchpath sub-segmentation engine that would facilitate more complex gestures,
such as multi-segment flicks and/or swipe-style input.  The decision was made to "ice" it for now in favor of preventing
further delays for the release of the feature.

To revisit a state when the system was fully connected, the following two commits may provide a useful reference:
- https://github.com/keymanapp/keyman/tree/0c7ac4ff3caf612674602fefb7ff71350b13964a
  - The last feature-gestures commit with full subsegmentation integration
- https://github.com/keymanapp/keyman/tree/5c48cc5b9b127ab42a8055b7d960dbc4d39e4df7
  - See #7440 - its description details the basic process for creation of a asynchronous FSM for recognizing & constructing gestures based on subsegments produced by the subsegmentation engine.
  - The permalinked commit itself holds the code that said process would connect with.
  - This was never fully committed to feature-gestures; #7440 was directly based upon the "last feature-gestures
    commit..." mentioned above.
