/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * URL-style path manipulation functions (OS-independent)
 */
import path from 'path-browserify';
import { CompilerPathCallbacks } from "../compiler-callbacks.js";

/**
 * Helper to replace all backslashes with forward slashes,
 * before passing to posix path module
 */
export class UrlSubpathCompilerCallback implements CompilerPathCallbacks {
  private normalizeSlashes(path: string): string {
    return path.replaceAll(/\\/g, '/');
  }

  dirname(name: string): string {
    return path.dirname(this.normalizeSlashes(name));
  }

  extname(name: string): string {
    return path.extname(this.normalizeSlashes(name));
  }

  basename(name: string, ext?: string): string {
    return path.basename(this.normalizeSlashes(name), ext);
  }

  isAbsolute(name: string): boolean {
    return path.isAbsolute(this.normalizeSlashes(name));
  }

  join(...paths: string[]): string {
    return path.join(...paths.map(this.normalizeSlashes));
  }

  normalize(p: string): string {
    return path.normalize(this.normalizeSlashes(p));
  }

  relative(from: string, to: string): string {
    return path.relative(this.normalizeSlashes(from), this.normalizeSlashes(to));
  }

  resolve(...args: string[]): string {
    return path.resolve(...args.map(this.normalizeSlashes));
  }
}
