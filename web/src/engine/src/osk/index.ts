// TODO-web-core: why do we export these types from web-utils and keyboard here?
export { DeviceSpec } from 'keyman/common/web-utils';
export { Codes, JSKeyboard, KeyboardProperties, SpacebarText } from 'keyman/engine/keyboard';

export { OSKView, JSKeyboardData } from './views/oskView.js';
export { FloatingOSKView, FloatingOSKViewConfiguration } from './views/floatingOskView.js';
export { AnchoredOSKView } from './views/anchoredOskView.js';
export { InlinedOSKView } from './views/inlinedOskView.js';
export { type KeyEventResultCallback, type KeyEventHandler, KeyEventSourceInterface } from './views/keyEventSource.interface.js';
export { BannerController } from './banner/bannerController.js';
// Is referenced by at least one desktop UI module.
export { FloatingOSKCookie as FloatingOSKViewCookie } from './views/floatingOskCookie.js';
export { VisualKeyboard } from './visualKeyboard.js';
export { type ViewConfiguration } from './config/viewConfiguration.js';
export { type KeyElement } from './keyElement.js';
export { type OSKBaseKey } from './keyboard-layout/oskBaseKey.js';
export { type GlobeHint } from './globehint.interface.js';
export { type KeyTip } from './keytip.interface.js';
export { type EmbeddedGestureConfig } from './config/embeddedGestureConfig.js';

export { getViewportScale } from './screenUtils.js';

export { Activator, StaticActivator } from './views/activator.js';
export { SimpleActivator } from './views/simpleActivator.js';
export { TwoStateActivator } from './views/twoStateActivator.js';
export { ParsedLengthStyle } from './lengthStyle.js';

export { gestureSetForLayout, DEFAULT_GESTURE_PARAMS } from './input/gestures/specsForLayout.js'

export { PredictionContext } from 'keyman/engine/interfaces';

export { keyTouchDistances, distributionFromDistanceMaps } from './corrections.js';
export {
    CorrectionLayoutEntry, CorrectionLayout, CorrectiveBaseKeyLayout,
    correctionKeyFilter, buildCorrectiveLayout
} from './correctionLayout.js';

// TODO-web-core: use a unitTestEndpoints pattern here (#15292)
export * as testIndex from './test-index.js';

// More things will likely need to be added.