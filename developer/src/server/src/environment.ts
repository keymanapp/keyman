/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Environmental variables and paths
 */
import path from 'node:path';
import { extractVersionData } from './version-data.js';
// TODO: environment should be just KEYMAN_VERSION

import KEYMAN_VERSION from "@keymanapp/keyman-version";
export const environment = extractVersionData(KEYMAN_VERSION.VERSION_WITH_TAG);

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
