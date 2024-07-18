import { KeymanSentry } from '@keymanapp/developer-utils';
import { readFileSync, existsSync } from 'fs';

export function loadJsonFile(filename: string): any {
  try {
    return existsSync(filename) ?
      JSON.parse(readFileSync(filename, 'utf-8')) :
      null;
  } catch(e: any) {
    console.error(`Error loading ${filename}: ${(e ?? '').toString()}`);
    KeymanSentry.reportException(e);
    return null;
  }
}