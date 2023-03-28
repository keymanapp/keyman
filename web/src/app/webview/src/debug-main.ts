import KeymanEngine from './keymanEngine.js'
import { SourcemappedWorker } from '@keymanapp/lexical-model-layer/web'

window['keyman'] = new KeymanEngine(SourcemappedWorker.constructInstance(), window.location.href);