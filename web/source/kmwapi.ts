/// <reference path="closure.ts" />
/// <reference path="kmwbase.ts" />
/// <reference path="kmwutils.ts" />
/// <reference path="kmwcallback.ts" />

// Allows proper minification handling.
/// <reference path="../node_modules/google-closure-library/closure/goog/base.js" />

/**
 * This file provides calls needed to export our established API elements when performing minification.
 * Closure can work with these instructions to prevent it from eliminating these names.
 * 
 * Note that in general, class fields in need of exposure cannot have their minification blocked;
 * maintenance of those must be performed manually, hence certain setters and this['propertyName']
 * code lines.
 * 
 * Format:  goog.exportSymbol("<API name>", <actual function name>);
 *     - <API name> - not minified, must match published API.
 *     - <actual function name> - may be renamed by Closure, no need to directly match the API name.  
 *       It should be a method of the same object, however, due to how 'this' works in JS.
 */

// kmwbase.ts
goog.exportSymbol("KeymanBase.prototype.addEventListener", KeymanBase.prototype.addEventListener);
goog.exportSymbol("KeymanBase.prototype.isFontAvailable", KeymanBase.prototype.isFontAvailable);

// Util.ts

goog.exportSymbol("Device", Device);
goog.exportSymbol("Util", Util);
goog.exportSymbol("Util.prototype.attachDOMEvent", Util.prototype.attachDOMEvent);
goog.exportSymbol("Util.prototype.detachDOMEvent", Util.prototype.detachDOMEvent);
goog.exportSymbol("Util.prototype.getOption", Util.prototype.getOption);
goog.exportSymbol("Util.prototype.setOption", Util.prototype.setOption);
goog.exportSymbol("Util.prototype.getAbsoluteX", Util.prototype._GetAbsoluteX);
goog.exportSymbol("Util.prototype.getAbsoluteY", Util.prototype._GetAbsoluteY);
goog.exportSymbol("Util.prototype.getAbsolute", Util.prototype._GetAbsolute);
goog.exportSymbol("Util.prototype.createElement", Util.prototype.createElement);
goog.exportSymbol("Util.prototype.getIEVersion", Util.prototype.getIEVersion);
goog.exportSymbol("Util.prototype.isTouchDevice", Util.prototype.isTouchDevice);
goog.exportSymbol("Util.prototype.createShim", Util.prototype.createShim);
goog.exportSymbol("Util.prototype.showShim", Util.prototype.showShim);
goog.exportSymbol("Util.prototype.hideShim", Util.prototype.hideShim);
goog.exportSymbol("Util.prototype.rgba", Util.prototype.rgba);
goog.exportSymbol("Util.prototype.addStyleSheet", Util.prototype.addStyleSheet);
goog.exportSymbol("Util.prototype.removeStyleSheet", Util.prototype.removeStyleSheet);
goog.exportSymbol("Util.prototype.linkStyleSheet", Util.prototype.linkStyleSheet);
goog.exportSymbol("Util.prototype.addFontFaceStyleSheet", Util.prototype.addFontFaceStyleSheet);
goog.exportSymbol("Util.prototype.loadCookie", Util.prototype.loadCookie);
goog.exportSymbol("Util.prototype.saveCookie", Util.prototype.saveCookie);
goog.exportSymbol("Util.prototype.toNumber", Util.prototype.toNumber);
goog.exportSymbol("Util.prototype.toFloat", Util.prototype.toFloat);
goog.exportSymbol("Util.prototype.toNzString", Util.prototype.nzString);
goog.exportSymbol("Util.prototype.alert", Util.prototype.alert);

// kmwcallback.ts
(function() {
  var exportKBCallback = function(miniName: string, longName: string, func:() => any) {
    goog.exportSymbol("KeyboardInterface.prototype." + longName, func);
    goog.exportSymbol("KeyboardInterface.prototype." + miniName, func);
  }

  var exportKBCallbackWithArgs = function(miniName: string, longName: string, func:(...args: any[]) => any) {
    goog.exportSymbol("KeyboardInterface.prototype." + longName, func);
    goog.exportSymbol("KeyboardInterface.prototype." + miniName, func);
  }

  exportKBCallback('KSF', 'saveFocus', KeyboardInterface.prototype.saveFocus);
  exportKBCallback('KBR', 'beepReset', KeyboardInterface.prototype.beepReset);

  exportKBCallbackWithArgs('KT', 'insertText', KeyboardInterface.prototype.insertText);
  exportKBCallbackWithArgs('KR', 'registerKeyboard', KeyboardInterface.prototype.registerKeyboard);
  exportKBCallbackWithArgs('KRS', 'registerStub', KeyboardInterface.prototype.registerStub);
  exportKBCallbackWithArgs('KC', 'context', KeyboardInterface.prototype.context);
  exportKBCallbackWithArgs('KN', 'nul', KeyboardInterface.prototype.nul);
  exportKBCallbackWithArgs('KCM', 'contextMatch', KeyboardInterface.prototype.contextMatch);
  exportKBCallbackWithArgs('KFCM', 'fullContextMatch', KeyboardInterface.prototype.fullContextMatch);
  exportKBCallbackWithArgs('KIK', 'isKeypress', KeyboardInterface.prototype.isKeypress);
  exportKBCallbackWithArgs('KKM', 'keyMatch', KeyboardInterface.prototype.keyMatch);
  exportKBCallbackWithArgs('KSM', 'stateMatch', KeyboardInterface.prototype.stateMatch);
  exportKBCallbackWithArgs('KKI', 'keyInformation', KeyboardInterface.prototype.keyInformation);
  exportKBCallbackWithArgs('KDM', 'deadkeyMatch', KeyboardInterface.prototype.deadkeyMatch);
  exportKBCallbackWithArgs('KB', 'beep', KeyboardInterface.prototype.beep);
  exportKBCallbackWithArgs('KA', 'any', KeyboardInterface.prototype.any);
  exportKBCallbackWithArgs('KO', 'output', KeyboardInterface.prototype.output);
  exportKBCallbackWithArgs('KDO', 'deadkeyOutput', KeyboardInterface.prototype.deadkeyOutput);
  exportKBCallbackWithArgs('KIO', 'indexOutput', KeyboardInterface.prototype.indexOutput);
  exportKBCallbackWithArgs('KIFS', 'ifStore', KeyboardInterface.prototype.ifStore);
  exportKBCallbackWithArgs('KSETS', 'setStore', KeyboardInterface.prototype.setStore);
  exportKBCallbackWithArgs('KLOAD', 'loadStore', KeyboardInterface.prototype.loadStore);
  exportKBCallbackWithArgs('KSAVE', 'saveStore', KeyboardInterface.prototype.saveStore);
}());