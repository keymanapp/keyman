/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Environmental variables and paths
 */
import path from 'node:path';

/**
 * @returns base path for the running server -- where index.js is stored
 */
export function serverBasePath() {
  return path.join(import.meta.dirname, '../../build/src');
}

/**
 * @returns path where the site public files can be found -- index.html
 */
export function serverSitePath() {
  return path.join(serverBasePath(), 'site');
}
