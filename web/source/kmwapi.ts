/// <reference path="kmwbase.ts" />
/// <reference path="kmwutils.ts" />
/// <reference path="text/kbdInterface.ts" />

/**
 * This file generates aliases linking renamed functions to some of our published developer API for KMW.
 * This won't enable Closure to do "advanced minification", but it's useful for ensuring we don't break
 * things people depended on in legacy versions.
 */

// Util.ts
(function() {
  let prototype = com.keyman.Util.prototype;

  var publishAPI = function(miniName: string, longName: string) {
    prototype[miniName] = prototype[longName];
  }

  publishAPI('getAbsoluteX', "_GetAbsoluteX");
  publishAPI("getAbsoluteY", "_GetAbsoluteY");
  publishAPI("getAbsolute", "_GetAbsolute");
  publishAPI("toNzString", "nzString");
}());

// Keyboard callbacks
(function() {
  let prototype = com.keyman.text.KeyboardInterface.prototype;

  var exportKBCallback = function(miniName: string, longName: string) {
    prototype[miniName] = prototype[longName];
  }

  exportKBCallback('KSF', 'saveFocus');
  exportKBCallback('KBR', 'beepReset');
  exportKBCallback('KT', 'insertText');
  exportKBCallback('KR', 'registerKeyboard');
  exportKBCallback('KRS', 'registerStub');
  exportKBCallback('KC', 'context');
  exportKBCallback('KN', 'nul');
  exportKBCallback('KCM', 'contextMatch');
  exportKBCallback('KFCM', 'fullContextMatch');
  exportKBCallback('KIK', 'isKeypress');
  exportKBCallback('KKM', 'keyMatch');
  exportKBCallback('KSM', 'stateMatch');
  exportKBCallback('KKI', 'keyInformation');
  exportKBCallback('KDM', 'deadkeyMatch');
  exportKBCallback('KB', 'beep');
  exportKBCallback('KA', 'any');
  exportKBCallback('KDC', 'deleteContext');
  exportKBCallback('KO', 'output');
  exportKBCallback('KDO', 'deadkeyOutput');
  exportKBCallback('KIO', 'indexOutput');
  exportKBCallback('KIFS', 'ifStore');
  exportKBCallback('KSETS', 'setStore');
  exportKBCallback('KLOAD', 'loadStore');
  exportKBCallback('KSAVE', 'saveStore');
}());