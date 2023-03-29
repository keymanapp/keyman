import KeymanEngine from './keymanEngine.js'
import { SourcemappedWorker } from '@keymanapp/lexical-model-layer/web'

var href = window.location.href;

var pwd = href.substr(0, href.lastIndexOf('/')+1);
window['keyman'] = new KeymanEngine(SourcemappedWorker.constructInstance(), pwd);