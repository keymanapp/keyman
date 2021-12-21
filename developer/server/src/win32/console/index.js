const os = require('os');
const {showConsole, hideConsole} = require(os.arch() == 'ia32' ? "./node-hide-console-window" : "./node-hide-console-window.x64");

module.exports = {
    showConsole,
    hideConsole
}