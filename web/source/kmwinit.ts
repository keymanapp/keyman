/***
   KeymanWeb 11.0
   Copyright 2019 SIL International
***/

/********************************************************/
/*                                                      */
/* Automatically initialize keymanweb with defaults     */ 
/* after the page is fully loaded                       */ 
/*                                                      */
/********************************************************/

(function() {
  // Declare KeymanWeb object
  var keymanweb=window['keyman'];

  // We don't want to instantly init() in case this code is used via bookmarklet.
  var readyStateCheckInterval = window.setInterval(function() {
    if (document.readyState === "complete") {
      window.clearInterval(readyStateCheckInterval);
      keymanweb.init(null);
    }
  }, 10);
  
})();
