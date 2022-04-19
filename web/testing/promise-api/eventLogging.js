var loggingActive = false;
var loggingElement = null;

// ------- UI aspects -------

// Handles activation/deactivation of logging (via radio buttons)
function eventLogChange() {
  const logActiveRadio = document.getElementById('eventLog-on');
  const offLabel = document.getElementById('event-label-off');
  const onLabel = document.getElementById('event-label-on');

  const loggingDiv = document.getElementById('logZone');
  const hasLoggingText = !!loggingElement.value;

  const active = logActiveRadio.checked;
  loggingActive = active;

  offLabel.className = active ? '' : 'active';
  onLabel.className  = active ? 'active' : '';

  loggingDiv.style.display = active || hasLoggingText ? 'block' : 'none';
}

// Exactly what it says - flushes / clears the logged-events textbox.
function flushLogs() {
  loggingElement.value = '';

  eventLogChange();
}

// UI + initialization
window.addEventListener('load', function() {
  let alertLink = null;
  if(alertType) {
    alertLink = document.getElementById('alert_default');
  } else {
    alertLink = document.getElementById('alert_disabled');
  }

  alertLink.className += 'active';
  loggingElement = document.getElementById('eventLogging');

  attachLoggingEvents();
});

// ------- Logging aspects -------

function attachLoggingEvents() {
  keyman.addEventListener('beforekeyboardchange', beforeChange);
  keyman.addEventListener('keyboardchange', change);
  keyman.addEventListener('keyboardloaded', loaded);
  keyman.addEventListener('keyboardregistered', registered);
}

// Gives us a nice, console.log-ish way to output to the textbox.
function appendLog(logText) {
  if(loggingElement.value) {
    logText = '\n' + logText;
  }
  loggingElement.value += logText;
}

// The handlers attached within attachLoggingEvents().
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