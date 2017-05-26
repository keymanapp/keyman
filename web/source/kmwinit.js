/***
   KeymanWeb 2.0
   Copyright 2017 SIL International
***/

/********************************************************/
/*                                                      */
/* Automatically initialize keymanweb with defaults     */ 
/* after the page is fully loaded                       */ 
/*                                                      */
/********************************************************/

(function()
{
  // Declare KeymanWeb object
  var keymanweb=window['tavultesoft']['keymanweb'];

  if(document.readyState === 'complete')
  {
    keymanweb.init(null);
  }
  else
  {
    var readyStateCheckInterval = window.setInterval(function() {
      if (document.readyState === "complete") 
      {
        window.clearInterval(readyStateCheckInterval);
        keymanweb.init(null);
      }
    }, 10);    
  }      
})();
