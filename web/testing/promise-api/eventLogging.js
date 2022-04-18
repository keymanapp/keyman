function attachLoggingEvents() {

  keyman.addEventListener('beforekeyboardchange', beforeChange);
  keyman.addEventListener('keyboardchange', change);
  keyman.addEventListener('keyboardloaded', loaded);
  keyman.addEventListener('keyboardregistered', registered);
}

function appendLog(logText) {
  if(this.loggingElement === undefined) {
    this.loggingElement = document.getElementById('eventLogging');
  }
  if(this.loggingElement.value) {
    logText = '\n' + logText;
  }
  this.loggingElement.value += logText;
}

function beforeChange(props) {
  if(!loggingActive) {
    return;
  }
  appendLog('beforekeyboardchange: ' + JSON.stringify(props));
  appendLog('- active keyboard: ' + keyman.getActiveKeyboard());
}

function change(props) {
  if(!loggingActive) {
    return;
  }
  appendLog('keyboardchange: ' + JSON.stringify(props));
  appendLog('- active keyboard: ' + keyman.getActiveKeyboard());
}

function loaded(props) {
  if(!loggingActive) {
    return;
  }
  appendLog('keyboardloaded: ' + JSON.stringify(props));
  appendLog('- active keyboard: ' + keyman.getActiveKeyboard());
}

function registered(props) {
  if(!loggingActive) {
    return;
  }
  appendLog('keyboardregistered: ' + JSON.stringify(props));
  appendLog('- active keyboard: ' + keyman.getActiveKeyboard());
}