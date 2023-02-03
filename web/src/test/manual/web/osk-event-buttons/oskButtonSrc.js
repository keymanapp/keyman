function addButton(name) {
  let enablerButton;
  let message;
  let eventName;

  if(name == "config") {
    enablerButton = document.getElementById("addConfig");
    message = "Config clicked";
    eventName = "configclick";
  } else {
    enablerButton = document.getElementById("addHelp");
    message = "Help clicked";
    eventName = "helpclick";
  }

  enablerButton.disabled = true;
  let logElement = document.getElementById("log");
  keyman.osk.addEventListener(eventName, () => {
    logElement.value += message + '\n';
  });
}