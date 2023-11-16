import { Banner } from "./banner.js";

/**
 * Function       BlankBanner
 * Description    A banner of height 0 that should not be shown
 */
export class BlankBanner extends Banner {
  readonly type = 'blank';

  constructor() {
    super(0);
  }
}