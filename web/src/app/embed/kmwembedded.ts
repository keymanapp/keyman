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
  keymanweb.mustReloadKeyboard = true; // pretty much tied to cache-busting, isn't it?
})();
