function updateConfig() {
  let layout = document.config.screen;
  let bounds = document.config.bounds;
  let receiver = document.config.receiver;

  let demoContainer = document.getElementById("demo-container");
  demoContainer.className = layout.value + " " + bounds.value + " " + receiver.value;
}

//////

window.addEventListener('load', function(ev) {
  let layoutGroup = document.config.screen;
  let boundsGroup = document.config.bounds;
  let receiverGroup = document.config.receiver;

  for(entry of layoutGroup) {
    entry.addEventListener('change', updateConfig);
  }

  for(entry of boundsGroup) {
    entry.addEventListener('change', updateConfig);
  }

  for(entry of receiverGroup) {
    entry.addEventListener('change', updateConfig);
  }

  updateConfig();
});