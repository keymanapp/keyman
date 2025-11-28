/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-11-26
 *
 * Keyboard names for KMC KMN Next Generation Compiler
 */

import { readdirSync, statSync } from 'fs';
import path from 'path';

export const PATH_TO_BASELINE = '../../../common/test/keyboards/baseline/';

export const BASELINE_KEYBOARD_NAMES = findKeyboardNames(PATH_TO_BASELINE);

export const PATH_TO_REPOSITORY = '../../../../keyboards/';

export const REPOSITORY_KEYBOARD_NAMES = findKeyboardNames(PATH_TO_REPOSITORY);

/**
 * Find the names of all the .kmn keyboard files in a directory
 * tree, excluding those in or below extras or legacy directories.
 * The names are provided without the initial base directory
 * path and without the .kmn file type.
 *
 * @param dir the directory to be searched
 * @param baseLength the length of the initial base directory
 * @param names an array of keyboard names
 * @returns the names
 */
function findKeyboardNames(dir: string, baseLength: number = dir.length, names: string[] = []): string[] {
  const files = readdirSync(dir);

  files.forEach((file) => {
    const filePath = path.join(dir, file);
    if (statSync(filePath).isDirectory() && !/extras$/.test(dir) && !/legacy$/.test(dir)) {
      findKeyboardNames(filePath, baseLength, names);
    } else if (/\.kmn$/.test(file)) {
      names.push(filePath.slice(baseLength, -4)); // remove base directory and file type
    }
  });

  return names;
}
