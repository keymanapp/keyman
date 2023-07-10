import type { ParsedLengthStyle } from "../lengthStyle.js";

export default interface OSKViewComponent {
  readonly element: HTMLElement;
  readonly layoutHeight: ParsedLengthStyle;
  refreshLayout(): void;
}