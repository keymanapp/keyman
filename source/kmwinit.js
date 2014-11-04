/***
   KeymanWeb 2.0
   Copyright 2014 Tavultesoft Pty Ltd

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
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
