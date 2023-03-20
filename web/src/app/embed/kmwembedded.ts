// KeymanWeb 11.0
// Copyright 2019 SIL International

/*****************************************/
/*                                       */
/*   Embedded application-specific code  */
/*                                       */
/*****************************************/

(function() {
  // Declare KeymanWeb and related objects
  var keymanweb=window['keyman'];

  // Flag to control refreshing of a keyboard that is already loaded
  keymanweb.mustReloadKeyboard = true;

  /**
   * Set target element text direction (LTR or RTL): not functional for KMEI, KMEA
   *
   * @param       {Object}      Ptarg      Target element
   */
  keymanweb.domManager._SetTargDir = function(Ptarg){};
})();
