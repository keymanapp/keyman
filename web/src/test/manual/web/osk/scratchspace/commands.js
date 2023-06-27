// Reference to the currently-active OSK.
var currentOSK = null;
var currentOSKMode = null;

// The currently-selected keyboard + metadata pairing.
var keyboard = null; // Cannot immediately initialized, as module-scripts use defer loading.

function log(str) {
  document.getElementById('eventlog').textContent += str + '\n\n';
}

// NOT a module - so we can declare a few event listener methods.
function setOSK(mode) {
  let modeList = ['floating', 'anchored', 'inline'];

  if(currentOSK) {
    if(mode) {
      if(!modeList.includes(mode)) {
        console.warn(`Invalid OSK mode requested: "${mode}"`);
      }
    }

    // Close out the previous one.
    currentOSK.shutdown();
    if(currentOSK.element) {
      try {
        currentOSK.element.remove();
      } catch(err) {
        console.error(err);
      }
    }
  }

  showOSK(true); // Reset related CSS classes for UI elements.

  modeList.forEach((panel) => {
    let panelElement = document.getElementById(panel + "-mode");

    if(panel == mode) {
      panelElement.classList.remove('inactive');
      panelElement.classList.add('active');
    } else {
      panelElement.classList.remove('active');
      panelElement.classList.add('inactive');
    }
  });

  let config = {
    ...BaseConfiguration,
    device: window.targetDevice,
    hostDevice: window.HOST_DEVICE
  };

  // Find the buttons & highlight accordingly.
  const oskGroup = document.getElementById('osk-select');
  const buttons = ['floating', 'anchored', 'inline'];
  highlightGroupSelection(oskGroup, buttons, mode);

  // Possible if there's no active OSK yet.
  currentOSK = null;
  currentOSKMode = mode;
  if(!mode) {
    return;
  }

  // Set up the new one.
  switch(mode) {
    case 'floating':
      currentOSK = new modules.osk.FloatingOSKView(config);
      setTarget('text');
      break;
    case 'anchored':
      currentOSK = new modules.osk.AnchoredOSKView(config);
      setTarget('text');
      break;
    case 'inline':
      currentOSK = new modules.osk.InlinedOSKView(config);
      document.getElementById('inline-container').appendChild(currentOSK.element);

      // Default:  8px for most browsers.
      const bodyMargin = parseInt(getComputedStyle(document.body).margin, 10);
      const maxWidth = screen.width - 2 * bodyMargin;

      // Check page width & constrain the OSK's width if necessary
      let targetWidth = 800;
      if(targetWidth > maxWidth) {
        targetWidth = maxWidth;
      }

      // Finally, set the OSK's size.
      currentOSK.setSize(targetWidth, targetWidth / 2);
      break;
    default:
  }

  if(!keyboard) {
    keyboard = setKeyboard('us');
  }
  currentOSK.activeKeyboard = keyboard;
  currentOSK.on('keyevent', (event) => {
    let eventText = JSON.stringify(event, (key, value) => {
      switch(key) {
        case 'srcKeyboard':
        case 'source':
        case 'device':
        case 'keyDistribution':
          return undefined;
      }

      return value;
    });
    log(eventText);
  });

  currentOSK.on('shouldShowLanguageMenu', () => {
    log("(language menu requested)");
  });

  currentOSK.on('onshow', () => {
    log("(osk shown)");
  });

  currentOSK.on('onhide', () => {
    log("(osk hidden)");
  })
}

function highlightGroupSelection(group, names, selected) {
    for(let i = 0; i < names.length; i++) {
      const button = group.children.item(i);

      if(names[i] == selected) {
        button.classList.add('selected');
      } else {
        button.classList.remove('selected');
      }
    }

}

function inlineSetSize() {
  currentOSK.setSize(document.getElementById('set-width').value + 'px', document.getElementById('set-height').value + 'px');
}

function setTarget(type) {
  let target = null;
  const textarea = document.getElementById('ta1');
  const input = document.getElementById('in1');

  textarea.classList.remove('selected');
  input.classList.remove('selected');

  // Find the buttons & highlight accordingly.
  const floatTarget = document.getElementById('float-target');
  const anchorTarget = document.getElementById('anchor-target');
  const buttons = ['text', 'input', 'none'];
  highlightGroupSelection(floatTarget, buttons, type);
  highlightGroupSelection(anchorTarget, buttons, type);

  if(type == 'text') {
    target = textarea;
  } else if(type == 'input') {
    target = input;
  }

  currentOSK.activationModel.activationTrigger = target;
  if(target) {
    target.classList.add('selected');
  }
}

function showOSK(mode) {
  // Find the buttons & highlight accordingly.
  const floatShow = document.getElementById('float-show');
  const anchorShow = document.getElementById('anchor-show');
  const buttons = [true, false];
  highlightGroupSelection(floatShow, buttons, mode);
  highlightGroupSelection(anchorShow, buttons, mode);

  if(!currentOSK) {
    return;
  }

  const visibleAtStart = currentOSK.isVisible();

  if(mode !== undefined) {
    currentOSK.show(mode);
  } else {
    currentOSK.show();
    highlightGroupSelection(floatShow, buttons, !visibleAtStart);
    highlightGroupSelection(anchorShow, buttons, visibleAtStart);
  }
}

function setKeyboard(kbdId) {
  if(kbdId) {
    const keyboardGroup = document.getElementById('keyboard-select');
    const buttons = ['us', 'lao_2008_basic', 'obolo_chwerty_6351'];
    highlightGroupSelection(keyboardGroup, buttons, kbdId);

    keyboard = window.keyboards[kbdId];
  } else if(!window.keyboard) {
    setKeyboard('us');
    return;
  }

  // Build the layer commands.
  const layerButtonGroup = document.getElementById('layer-select');
  const layout = keyboard.keyboard.layout(window.targetDevice.formFactor);
  const layerNames = Object.keys(layout.layerMap);
  layerButtonGroup.innerHTML = ''; // clear all children
  for(let layer of layerNames) {
    const button = document.createElement('button');
    button.textContent = layer;
    button.onclick = () => {
      if(currentOSK && currentOSK.vkbd) {
        currentOSK.vkbd.layerId = layer;

        for(let i = 0; i < layerButtonGroup.children.length; i++) {
          const btn = layerButtonGroup.children.item(i);

          if(btn == button) {
            btn.classList.add('selected');
          } else {
            btn.classList.remove('selected');
          }
        }
      }
    }

    if(layer == 'default') {
      button.classList.add('selected');
    }

    layerButtonGroup.appendChild(button);
  }

  if(!currentOSK) {
    return keyboard;
  }

  // From here out, rely on the global var `keyboard`.
  currentOSK.activeKeyboard = keyboard;
  return keyboard;
}

function setTargetDevice(target) {
  const devices = {
    windows: new modules.osk.DeviceSpec('chrome', 'desktop', 'windows', false),
    aPhone: new modules.osk.DeviceSpec('chrome', 'phone', 'android', true),
    aPad: new modules.osk.DeviceSpec('chrome', 'tablet', 'android', true),
    iPhone: new modules.osk.DeviceSpec('safari', 'phone', 'ios', true),
    iPad: new modules.osk.DeviceSpec('safari', 'tablet', 'ios', true),
  };

  if(target) {
    const deviceGroup = document.getElementById('device-select');
    const buttons = ['windows', 'aPhone', 'aPad', 'iPhone', 'iPad'];
    highlightGroupSelection(deviceGroup, buttons, target);

    window.targetDevice = devices[target];
  }

  setOSK(currentOSKMode);
}