import { validateModelDefs } from './headless/gestures/specs/modelDefValidator.js';

export { AsyncClosureDispatchQueue, QueueClosure } from './headless/asyncClosureDispatchQueue.js';
export { CumulativePathStats } from './headless/cumulativePathStats.js';
export { GestureModelDefs } from './headless/gestures/specs/gestureModelDefs.js';
export { GestureRecognizer } from "./gestureRecognizer.js";
export { GestureRecognizerConfiguration } from "./configuration/gestureRecognizerConfiguration.js";
export { InputEngineBase } from "./headless/inputEngineBase.js";
export { InputSample } from "./headless/inputSample.js";
export { GesturePath } from "./headless/gesturePath.js";
export { GestureDebugPath } from "./headless/gestureDebugPath.js"
export { ConfigChangeClosure, GestureStageReport, GestureSequence } from "./headless/gestures/matchers/gestureSequence.js";
export { GestureSource, GestureSourceSubview, buildGestureMatchInspector, SerializedGestureSource } from "./headless/gestureSource.js";
export { GestureDebugSource } from "./headless/gestureDebugSource.js";
export { MouseEventEngine } from "./mouseEventEngine.js";
export { PaddedZoneSource } from './configuration/paddedZoneSource.js';
export { RecognitionZoneSource } from './configuration/recognitionZoneSource.js';
export { SegmentClassifier } from "./headless/segmentClassifier.js";
export { TouchEventEngine } from "./touchEventEngine.js";
export { TouchpointCoordinator } from "./headless/touchpointCoordinator.js";
export { ViewportZoneSource } from './configuration/viewportZoneSource.js';

export * as gestures from './headless/gestures/index.js';
export { validateModelDefs } from './headless/gestures/specs/modelDefValidator.js';

// TODO:  export other odds & ends.