/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Output artifacts available from kmc-convert
 */
import { KeymanCompilerArtifactOptional, KeymanCompilerArtifacts } from '@keymanapp/developer-utils';
import { KeymanCompilerResult } from "@keymanapp/developer-utils";

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


export interface ConverterResult extends KeymanCompilerResult {
  /**
   * Internal in-memory build artifacts from a successful compilation. Caller
   * can write these to disk with {@link Converter.write}
   */
  artifacts: ConverterArtifacts;
};

export interface ConverterToKmnResult extends ConverterResult {
  /**
   * Internal in-memory build artifacts from a successful compilation to kmn.
   * Caller can write these to disk with {@link Converter.write}
   */
  artifacts: ConverterToKmnArtifacts;
};
