/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Output artifacts available from kmc-convert
 */
import { KeymanCompilerArtifactOptional, KeymanCompilerArtifacts } from '@keymanapp/developer-utils';

export interface ConverterArtifacts extends KeymanCompilerArtifacts { }

/**
 * @public
 * Internal in-memory build artifacts from a successful compilation
 */
export interface ConverterToKmnArtifacts extends ConverterArtifacts {
  /**
   * Source keyboard filedata and filename
   */
  kmn?: KeymanCompilerArtifactOptional;
  /**
   * Source on screen keyboard filedata and filename
   */
  kvks?: KeymanCompilerArtifactOptional;
  /**
   * Source touch keyboard filedata and filename
   */
  keymanTouchLayout?: KeymanCompilerArtifactOptional;
};
