export { Codes, DeviceSpec, JSKeyboard, KeyboardProperties, SpacebarText } from 'keyman/engine/keyboard';

export { default as OSKView } from './views/oskView.js';
export { default as FloatingOSKView, FloatingOSKViewConfiguration } from './views/floatingOskView.js';
export { default as AnchoredOSKView } from './views/anchoredOskView.js';
export { default as InlinedOSKView } from './views/inlinedOskView.js';
export { type KeyEventResultCallback, type KeyEventHandler, KeyEventSourceInterface } from './views/keyEventSource.interface.js';
export { BannerController } from './banner/bannerController.js';
// Is referenced by at least one desktop UI module.
export { FloatingOSKCookie as FloatingOSKViewCookie } from './views/floatingOskCookie.js';
export { default as VisualKeyboard } from './visualKeyboard.js';
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

export { PredictionContext } from 'keyman/engine/interfaces';

export * from './corrections.js';
export * from './correctionLayout.js';

// More things will likely need to be added.