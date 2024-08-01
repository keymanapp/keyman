export { AttachmentInfo } from './attachmentInfo.js';

/*
 * Note:  for `instanceof` the attachment objects returned by the next module's
 * to match that of the actual objects within the browser when bundled, the
 * **same bundle** must contain reference points for those classes' definitions.
 */
export { eventOutputTarget, outputTargetForElement } from './outputTargetForElement.js';
export { PageContextAttachment, PageAttachmentOptions } from './pageContextAttachment.js';

/*
 * Following from the prior "Note:", we republish `engine/element-wrappers` here -
 * this matters quite strongly for certain unit tests.
 */
export * from 'keyman/engine/element-wrappers';