import { deepCopy } from 'keyman/common/web-utils';

import {
  gestures,
  GestureModelDefs
} from 'keyman/engine/gesture-processor';

import { BannerSuggestion } from './suggestionBanner.js';
import { simpleTapModelWithReset } from "../input/gestures/specsForLayout.js";

export const BannerSimpleTap: gestures.specs.GestureModel<BannerSuggestion> = {
  ...deepCopy(simpleTapModelWithReset(null)),
  resolutionAction: {
    type: 'complete',
    item: 'current'
  }
};

export const BANNER_GESTURE_SET: GestureModelDefs<BannerSuggestion> = {
  gestures: [
    BannerSimpleTap
  ],
  sets: {
    default: [BannerSimpleTap.id]
  }
}