/// <reference path="closure.ts" />
/// <reference path="kmwbase.ts" />
/// <reference path="kmwutils.ts" />
/// <reference path="text/kbdInterface.ts" />

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

// Util.ts

// goog.exportSymbol("Device", Device);
// goog.exportSymbol("Util", Util);
goog.exportSymbol("com.keyman.Util.prototype.attachDOMEvent", com.keyman.Util.prototype.attachDOMEvent);
goog.exportSymbol("com.keyman.Util.prototype.detachDOMEvent", com.keyman.Util.prototype.detachDOMEvent);
goog.exportSymbol("com.keyman.Util.prototype.getOption", com.keyman.Util.prototype.getOption);
goog.exportSymbol("com.keyman.Util.prototype.setOption", com.keyman.Util.prototype.setOption);
goog.exportSymbol("com.keyman.Util.prototype.getAbsoluteX", com.keyman.Util.prototype.getAbsoluteX);
goog.exportSymbol("com.keyman.Util.prototype.getAbsoluteY", com.keyman.Util.prototype.getAbsoluteY);
goog.exportSymbol("com.keyman.Util.prototype.getAbsolute", com.keyman.Util.prototype._GetAbsolute);
goog.exportSymbol("com.keyman.Util.prototype.createElement", com.keyman.Util.prototype.createElement);
goog.exportSymbol("com.keyman.Util.prototype.getIEVersion", com.keyman.Util.prototype.getIEVersion);
goog.exportSymbol("com.keyman.Util.prototype.isTouchDevice", com.keyman.Util.prototype.isTouchDevice);
goog.exportSymbol("com.keyman.Util.prototype.createShim", com.keyman.Util.prototype.createShim);
goog.exportSymbol("com.keyman.Util.prototype.showShim", com.keyman.Util.prototype.showShim);
goog.exportSymbol("com.keyman.Util.prototype.hideShim", com.keyman.Util.prototype.hideShim);
goog.exportSymbol("com.keyman.Util.prototype.rgba", com.keyman.Util.prototype.rgba);
goog.exportSymbol("com.keyman.Util.prototype.addStyleSheet", com.keyman.Util.prototype.addStyleSheet);
goog.exportSymbol("com.keyman.Util.prototype.removeStyleSheet", com.keyman.Util.prototype.removeStyleSheet);
goog.exportSymbol("com.keyman.Util.prototype.linkStyleSheet", com.keyman.Util.prototype.linkStyleSheet);
goog.exportSymbol("com.keyman.Util.prototype.addFontFaceStyleSheet", com.keyman.Util.prototype.addFontFaceStyleSheet);
goog.exportSymbol("com.keyman.Util.prototype.loadCookie", com.keyman.Util.prototype.loadCookie);
goog.exportSymbol("com.keyman.Util.prototype.saveCookie", com.keyman.Util.prototype.saveCookie);
goog.exportSymbol("com.keyman.Util.prototype.toNumber", com.keyman.Util.prototype.toNumber);
goog.exportSymbol("com.keyman.Util.prototype.toFloat", com.keyman.Util.prototype.toFloat);
goog.exportSymbol("com.keyman.Util.prototype.toNzString", com.keyman.Util.prototype.nzString);
goog.exportSymbol("com.keyman.Util.prototype.alert", com.keyman.Util.prototype.alert);

// kmwcallback.ts
(function() {
  var exportKBCallback = function(miniName: string, longName: string, func:() => any) {
    goog.exportSymbol("com.keyman.KeyboardInterface.prototype." + longName, func);
    goog.exportSymbol("com.keyman.KeyboardInterface.prototype." + miniName, func);
  }

  var exportKBCallbackWithArgs = function(miniName: string, longName: string, func:(...args: any[]) => any) {
    goog.exportSymbol("com.keyman.text.KeyboardInterface.prototype." + longName, func);
    goog.exportSymbol("com.keyman.text.KeyboardInterface.prototype." + miniName, func);
  }

  exportKBCallback('KSF', 'saveFocus', com.keyman.text.KeyboardInterface.prototype.saveFocus);
  exportKBCallback('KBR', 'beepReset', com.keyman.text.KeyboardInterface.prototype.beepReset);

  exportKBCallbackWithArgs('KT', 'insertText', com.keyman.text.KeyboardInterface.prototype.insertText);
  exportKBCallbackWithArgs('KR', 'registerKeyboard', com.keyman.text.KeyboardInterface.prototype.registerKeyboard);
  exportKBCallbackWithArgs('KRS', 'registerStub', com.keyman.text.KeyboardInterface.prototype.registerStub);
  exportKBCallbackWithArgs('KC', 'context', com.keyman.text.KeyboardInterface.prototype.context);
  exportKBCallbackWithArgs('KN', 'nul', com.keyman.text.KeyboardInterface.prototype.nul);
  exportKBCallbackWithArgs('KCM', 'contextMatch', com.keyman.text.KeyboardInterface.prototype.contextMatch);
  exportKBCallbackWithArgs('KFCM', 'fullContextMatch', com.keyman.text.KeyboardInterface.prototype.fullContextMatch);
  exportKBCallbackWithArgs('KIK', 'isKeypress', com.keyman.text.KeyboardInterface.prototype.isKeypress);
  exportKBCallbackWithArgs('KKM', 'keyMatch', com.keyman.text.KeyboardInterface.prototype.keyMatch);
  exportKBCallbackWithArgs('KSM', 'stateMatch', com.keyman.text.KeyboardInterface.prototype.stateMatch);
  exportKBCallbackWithArgs('KKI', 'keyInformation', com.keyman.text.KeyboardInterface.prototype.keyInformation);
  exportKBCallbackWithArgs('KDM', 'deadkeyMatch', com.keyman.text.KeyboardInterface.prototype.deadkeyMatch);
  exportKBCallbackWithArgs('KB', 'beep', com.keyman.text.KeyboardInterface.prototype.beep);
  exportKBCallbackWithArgs('KA', 'any', com.keyman.text.KeyboardInterface.prototype.any);
  exportKBCallbackWithArgs('KDC', 'deleteContext', com.keyman.text.KeyboardInterface.prototype.deleteContext);
  exportKBCallbackWithArgs('KO', 'output', com.keyman.text.KeyboardInterface.prototype.output);
  exportKBCallbackWithArgs('KDO', 'deadkeyOutput', com.keyman.text.KeyboardInterface.prototype.deadkeyOutput);
  exportKBCallbackWithArgs('KIO', 'indexOutput', com.keyman.text.KeyboardInterface.prototype.indexOutput);
  exportKBCallbackWithArgs('KIFS', 'ifStore', com.keyman.text.KeyboardInterface.prototype.ifStore);
  exportKBCallbackWithArgs('KSETS', 'setStore', com.keyman.text.KeyboardInterface.prototype.setStore);
  exportKBCallbackWithArgs('KLOAD', 'loadStore', com.keyman.text.KeyboardInterface.prototype.loadStore);
  exportKBCallbackWithArgs('KSAVE', 'saveStore', com.keyman.text.KeyboardInterface.prototype.saveStore);
}());