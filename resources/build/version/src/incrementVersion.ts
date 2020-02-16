import { readFileSync, writeFileSync } from 'fs';

// ------------------------------------------------------------------------------------
// incrementVersion
// ------------------------------------------------------------------------------------

export const incrementVersion = () => {
  // 1. 
  const version = readFileSync('./VERSION.md', 'utf8');
  const triplet = version.split('.');
  const patch = parseInt(triplet[2], 10) + 1;
  const newVersion = `${triplet[0]}.${triplet[1]}.${patch}`;
  writeFileSync('./VERSION.md', newVersion, 'utf8');
  return newVersion;
};
