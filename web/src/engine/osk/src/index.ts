export { DeviceSpec, Keyboard, KeyboardProperties, SpacebarText } from '@keymanapp/keyboard-processor';

export { default as OSKView } from './views/oskView.js';
export { default as FloatingOSKView, FloatingOSKViewConfiguration } from './views/floatingOskView.js';
export { default as AnchoredOSKView } from './views/anchoredOskView.js';
export { default as InlinedOSKView } from './views/inlinedOskView.js';
export { BannerController } from './banner/bannerController.js';
// Is referenced by at least one desktop UI module.
export { FloatingOSKCookie as FloatingOSKViewCookie } from './views/floatingOskCookie.js';
export { default as VisualKeyboard } from './visualKeyboard.js';
export { type default as OSKResourcePathConfiguration } from './config/oskResourcePathConfiguration.interface.js';
export { type default as ViewConfiguration } from './config/viewConfiguration.js';
export { type KeyElement } from './keyElement.js';
export { type default as OSKBaseKey } from './keyboard-layout/oskBaseKey.js';
export { type default as GlobeHint } from './globehint.interface.js';
export { type default as KeyTip } from './keytip.interface.js';
export { type default as EmbeddedGestureConfig } from './config/embeddedGestureConfig.js';

export { getViewportScale } from './screenUtils.js';

export { default as Activator, StaticActivator } from './views/activator.js';
export { default as SimpleActivator } from './views/simpleActivator.js';
export { default as TwoStateActivator } from './views/twoStateActivator.js';
export { ParsedLengthStyle } from './lengthStyle.js';

export { gestureSetForLayout, DEFAULT_GESTURE_PARAMS } from './input/gestures/specsForLayout.js'

// PredictionContext is exported from input-processor, not the OSK.

// More things will likely need to be added.