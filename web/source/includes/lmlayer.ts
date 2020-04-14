/**
 * Defines the reference-path tags necessary to link properly with common/predictive-text.
 */

// Defines the main interface of the Language Modeling Layer (LMLayer) and its original typing information.
///<reference path="../../../common/predictive-text/embedded_worker.d.ts" />
///<reference path="../../../common/predictive-text/browser-index.ts" />

// We DO need the embedded_worker.d.ts file - since we're directly linking into the LMLayer's code,
// we need the typedef for the embedded worker to be linked.