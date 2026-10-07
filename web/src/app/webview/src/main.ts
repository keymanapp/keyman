/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import { KeymanEngine } from './keymanEngine.js'
import { WebPredictiveTextWorkerFactory } from '@keymanapp/lexical-model-layer/web'

/**
* Determine path and protocol of executing script, setting them as
* construction defaults.
*/
const ss = (document.currentScript as HTMLScriptElement)?.src;
const sPath = ss ? ss.substring(0, ss.lastIndexOf('/') + 1) : './';

// @ts-ignore
window['keyman'] = new KeymanEngine(new WebPredictiveTextWorkerFactory(sPath + 'worker-thread.js'), sPath);