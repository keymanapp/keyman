export { DeviceSpec } from '@keymanapp/web-utils/build/obj/deviceSpec.js';
export { default as Keyboard } from '@keymanapp/keyboard-processor/build/obj/keyboards/keyboard.js';
export { default as KeyboardProperties } from '@keymanapp/keyboard-processor/build/obj/keyboards/keyboardProperties.js';
export { default as Codes } from '@keymanapp/keyboard-processor/build/obj/text/codes.js';

export { default as FloatingOSKView } from './views/floatingOskView.js';
export { default as FloatingOSKViewCookie } from './views/floatingOskCookie.js';
export { default as AnchoredOSKView } from './views/anchoredOskView.js';
export { default as InlinedOSKView } from './views/inlinedOskView.js';
export { BannerController } from './banner/bannerView.js';
export { default as VisualKeyboard } from './visualKeyboard.js';
export type { default as ViewConfiguration } from './config/viewConfiguration.js';
export type { default as SpacebarText } from '@keymanapp/keyboard-processor/src/keyboards/spacebarText.js';

export { default as Activator } from './views/activator.js';
export { default as SimpleActivator } from './views/simpleActivator.js';
export { default as TwoStateActivator } from './views/twoStateActivator.js';
export { ParsedLengthStyle } from './lengthStyle.js';

// PredictionContext is exported from input-processor, not the OSK.

// More things will likely need to be added.