/**
 * Defines the reference-path tags necessary to link properly with common/predictive-text.
 */

// Defines the main interface of the Language Modeling Layer (LMLayer) and its original typing information.
///<reference path="../../node_modules/@keymanapp/language-modeling-layer/embedded_worker.d.ts" />
///<reference path="../../node_modules/@keymanapp/language-modeling-layer/web-defaultWorker.ts" />
///<reference path="../../node_modules/@keymanapp/language-modeling-layer/index.ts" />

// We DO need the embedded_worker.d.ts file - since we're directly linking into the LMLayer's code,
// we need the typedef for the embedded worker to be linked.