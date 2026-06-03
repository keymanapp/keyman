import type { ParsedLengthStyle } from "../lengthStyle.js";

export interface OSKViewComponent {
  readonly element: HTMLElement;
  readonly layoutHeight: ParsedLengthStyle;
  refreshLayout(): void;
}