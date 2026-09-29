/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-11-26
 *
 * Keyboard names for KMC KMN Next Generation Compiler
 */

import { existsSync, readdirSync, statSync } from 'node:fs';
import path from 'node:path';

export const PATH_TO_BASELINE = '../../../common/test/keyboards/baseline/';

let baselineKeyboardNamesCache: string[] = null;
export const baselineKeyboardNames = () => baselineKeyboardNamesCache ??= findKeyboardNames(PATH_TO_BASELINE);

export const PATH_TO_REPOSITORY = '../../../../keyboards/';

let repositoryKeyboardNamesCache: string[] = null;
export const repositoryKeyboardNames = () => repositoryKeyboardNamesCache ??= findKeyboardNames(PATH_TO_REPOSITORY);

const EXCLUDED_REPOSITORY_FILES = [
  // $keyman, $keymanonly, $keymanweb
  'experimental\\c\\chalchiteko\\source\\chalchiteko',
  'experimental\\gff\\gff_geez_emufi\\source\\gff_geez_emufi',
  'experimental\\s\\sp_lentan_ucsur\\source\\sp_lentan_ucsur',
  'experimental\\s\\sp_wakalito_ucsur\\source\\sp_wakalito_ucsur',
  'release\\c\\clavbur9\\source\\clavbur9',
  'release\\e\\eo_plus\\source\\eo_plus',
  'release\\g\\galaxie_greek_mnemonic\\source\\galaxie_greek_mnemonic',
  'release\\g\\galaxie_hebrew_mnemonic\\source\\galaxie_hebrew_mnemonic',
  'release\\gff\\gff_amharic\\source\\gff_amharic',
  'release\\gff\\gff_amharic_classic\\source\\gff_amharic_classic',
  'release\\gff\\gff_amh_7\\source\\gff_amh_7',
  'release\\gff\\gff_awngi_xamtanga\\source\\gff_awngi_xamtanga',
  'release\\gff\\gff_blin\\source\\gff_blin',
  'release\\gff\\gff_ethiopic\\source\\gff_ethiopic',
  'release\\gff\\gff_geez\\source\\gff_geez',
  'release\\gff\\gff_gurage\\source\\gff_gurage',
  'release\\gff\\gff_gurage_legacy\\source\\gff_gurage_legacy',
  'release\\gff\\gff_harari\\source\\gff_harari',
  'release\\gff\\gff_tigre\\source\\gff_tigre',
  'release\\gff\\gff_tigrinya_eritrea\\source\\gff_tigrinya_eritrea',
  'release\\gff\\gff_tigrinya_ethiopia\\source\\gff_tigrinya_ethiopia',
  'release\\h\\hieroglyphic\\source\\hieroglyphic',
  'release\\itrans\\itrans_bengali\\source\\itrans_bengali',
  'release\\itrans\\itrans_devanagari_hindi\\source\\itrans_devanagari_hindi',
  'release\\itrans\\itrans_devanagari_sanskrit_vedic\\source\\itrans_devanagari_sanskrit_vedic',
  'release\\itrans\\itrans_gujarati\\source\\itrans_gujarati',
  'release\\itrans\\itrans_odia\\source\\itrans_odia',
  'release\\k\\karakalpak_cyrillic\\source\\karakalpak_cyrillic',
  'release\\k\\karakalpak_latin\\source\\karakalpak_latin',
  'release\\k\\kbdsn1\\source\\kbdsn1',
  'release\\k\\korean_rr\\source\\korean_rr',
  'release\\m\\masaram_gondi\\source\\masaram_gondi',
  'release\\o\\old_hungarian\\source\\old_hungarian',
  'release\\o\\old_hungarian_carpathian_highlands\\source\\old_hungarian_carpathian_highlands',
  'release\\sil\\sil_cipher_music\\source\\sil_cipher_music',
  'release\\sil\\sil_euro_latin\\source\\sil_euro_latin',
  'release\\sil\\sil_ipa\\source\\sil_ipa',
  'release\\sil\\sil_mali_azerty\\source\\sil_mali_azerty',
  'release\\sil\\sil_mali_qwerty\\source\\sil_mali_qwerty',
  'release\\sil\\sil_pan_africa_mnemonic\\source\\sil_pan_africa_mnemonic',
  'release\\sil\\sil_tchad\\source\\sil_tchad'
];

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
  if (!existsSync(dir)) {
    return [];
  }

  const files = readdirSync(dir);

  files.forEach((file) => {
    const filePath = path.join(dir, file);
    if (statSync(filePath).isDirectory() && !/(extras|legacy)$/.test(filePath)) {
      findKeyboardNames(filePath, baseLength, names);
    } else if (/\.kmn$/.test(file)) {
      const fileName: string = filePath.slice(baseLength, -4); // remove base directory and file type
      if (EXCLUDED_REPOSITORY_FILES.indexOf(fileName) == -1) {
        names.push(fileName);
      }
    }
  });

  return names;
}
