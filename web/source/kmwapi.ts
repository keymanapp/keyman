/// <reference path="closure.ts" />
/// <reference path="kmwbase.ts" />
/// <reference path="kmwutils.ts" />

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
goog.exportSymbol("Util.prototype.getIEVersion", Util.prototype._GetIEVersion);
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