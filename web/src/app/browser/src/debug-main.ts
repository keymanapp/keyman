import KeymanEngine from './keymanEngine.js'
import { SourcemappedWorker } from '@keymanapp/lexical-model-layer/web'

 /**
  * Determine path and protocol of executing script, setting them as
  * construction defaults.
  *
  * This can only be done during load when the active script will be the
  * last script loaded.  Otherwise the script must be identified by name.
 */
 var scripts = document.getElementsByTagName('script');
 var ss = scripts[scripts.length-1].src;
 var sPath = ss.substr(0,ss.lastIndexOf('/')+1);

// @ts-ignore
window['keyman'] = new KeymanEngine(SourcemappedWorker.constructInstance(), sPath);