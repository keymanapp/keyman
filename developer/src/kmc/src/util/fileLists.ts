import * as fs from 'fs';
import * as path from 'path';
import { CompilerCallbacks } from "@keymanapp/developer-utils";
import { InfrastructureMessages } from "../messages/infrastructureMessages.js";

/**
 * Replaces each entry starting with `@` with the content of the file, with one
 * line per file, filenames trimmed, and any lines that are blank or starting
 * with `#` (marking a comment) removed. Note: `#` anywhere else is treated as
 * part of the filename.
 *
 * If any filelist does not exist, reports an error and returns false.
 *
 * @param filenames
 * @param callbacks
 * @returns false on failure
 */
export function expandFileLists(filenames: string[], callbacks: CompilerCallbacks) {
  let i = 0;
  while(i < filenames.length) {
    if(filenames[i].startsWith('@')) {
      const fileList = expandFileList(filenames[i].substring(1), callbacks);
      if(fileList === null) {
        return false;
      }
      filenames.splice(i, 1, ...fileList);
      i += fileList.length;
    } else {
      i++;
    }
  }
  return true;
}

export function expandFileList(filename: string, callbacks: CompilerCallbacks): string[] {
  if(!fs.existsSync(filename)) {
    callbacks.reportMessage(InfrastructureMessages.Error_FileDoesNotExist({filename}));
    return null;
  }

  const files = fs.readFileSync(filename, 'utf-8').split('\n').map(item => {
    item = item.trim();
    return item.startsWith('#') || item == ''
      ? ''
      : path.resolve(path.dirname(filename), item)
  }).filter(item => item.length > 0);

  return files;
}
