import KeymanEngine from './keymanEngine.js'
import { Worker } from '@keymanapp/lexical-model-layer/web'

window['keyman'] = new KeymanEngine(Worker.constructInstance(), window.location.href);