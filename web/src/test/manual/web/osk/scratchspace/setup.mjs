import * as KeymanOSK from '../../../../../../build/engine/osk/lib/index.mjs';
import Device from '../../../../../../build/engine/device-detect/lib/index.mjs';
import { loadKeyboardFromPath, loadKeyboardsFromStubs } from '../kbdLoader.mjs';

// NOTE:  we must actually set these in this manner to have accessibility outside of
//        this script tag!
window.modules = {
  osk: KeymanOSK,
  device: Device
};

let prefix = '../../';
const stubs = [
  {id:'us',name:'English',languages:{id:'en',name:'English'}, filename:(prefix + 'us-1.0.js')},
  {id:'obolo_chwerty_6351',name:'obolo_chwerty_6351',languages:{id:'en',name:'English'},
    filename:(prefix + 'obolo_chwerty_6351.js')},
  {id:'lao_2008_basic',name:'Lao Basic', languages: {id:'lo',name:'Lao',region:'Asia'},
    filename:(prefix + 'lao_2008_basic-1.2.js')}
]

window.keyboards = loadKeyboardsFromStubs(stubs).then((keyboards) => {
  window.keyboards = keyboards;
});

let device = new Device();
device.detect();

if(device.formFactor == 'phone') {
  document.body.classList.add('phone');
}

if(device.touchable) {
  document.body.classList.add('touch');
}

window.HOST_DEVICE = device.coreSpec;
window.targetDevice = new modules.osk.DeviceSpec('safari', 'phone', 'ios', true);

window.BaseConfiguration = {
  isEmbedded: false,
  // Relative path to the resources' original versions.
  pathConfig: {
    resources: '../../../../../resources',
    fonts: ''
  }
};

window.keyboardLoader = loadKeyboardFromPath;