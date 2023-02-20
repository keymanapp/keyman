/// <reference path="kmwbase.ts" />
/// <reference path="kmwutils.ts" />

/**
 * This file generates aliases linking renamed functions to some of our published developer API for KMW.
 * This won't enable Closure to do "advanced minification", but it's useful for ensuring we don't break
 * things people depended on in legacy versions.
 */

// Util.ts
(function() {
  let prototype = com.keyman.Util.prototype;

  var publishAPI = function(legacyName: string, name: string) {
    prototype[legacyName] = prototype[name];
  }

  // These four were renamed, but we need to maintain their legacy names.
  publishAPI("_GetAbsoluteX", 'getAbsoluteX');
  publishAPI("_GetAbsoluteY", "getAbsoluteY");
  publishAPI("_GetAbsolute", "getAbsolute");
  publishAPI("toNzString", "nzString");
}());

(function() {
  // DOM-aware KeymanWeb overwrites some of the API functions, so we
  // re-publish the API so that the overwritten functions are accessible
  // via their short-form equivalents found in actual keyboard code.
  com.keyman.text.KeyboardInterface.__publishShorthandAPI();
}());