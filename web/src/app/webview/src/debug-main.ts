import KeymanEngine from './keymanEngine.js'
import { SourcemappedWorker } from '@keymanapp/lexical-model-layer/web'

 /**
  * Determine path and protocol of executing script, setting them as
  * construction defaults.
  *
  * This can only be done during load when the active script will be the
  * last script loaded.  Otherwise the script must be identified by name.
 */
const scripts = document.getElementsByTagName('script');
const ss = scripts[scripts.length-1].src;
const sPath = ss.substring(0,ss.lastIndexOf('/')+1);

window['keyman'] = new KeymanEngine(SourcemappedWorker.constructInstance(), sPath);