/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { LexicalModelTypes } from '@keymanapp/common-types';

export type Alternate = LexicalModelTypes.ProbabilityMass<LexicalModelTypes.Transform>;

export class TextTransform implements LexicalModelTypes.Transform {
  readonly insert: string;
  readonly deleteLeft: number;
  readonly deleteRight: number;
  readonly erasedSelection: boolean;
  id: number;

  constructor(insert: string, deleteLeft: number, deleteRight: number, erasedSelection: boolean) {
    this.insert = insert;
    this.deleteLeft = deleteLeft;
    this.deleteRight = deleteRight;
    this.erasedSelection = erasedSelection;
  }

  public static readonly nil = new TextTransform('', 0, 0, false);
}
