import * as os from 'os';
import { createRequire } from 'node:module';
const require = createRequire(import.meta.url);
const {showConsole, hideConsole} = require(os.arch() == 'ia32' ? "./node-hide-console-window" : "./node-hide-console-window.x64");

export default { showConsole, hideConsole };
