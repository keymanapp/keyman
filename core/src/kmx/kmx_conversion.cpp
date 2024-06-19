


#include "kmx_processevent.h"

using namespace km::core;
using namespace kmx;

/* This array is lifted from preservedkeymap.cpp */

const struct
{
  KMX_DWORD key;
  KMX_BOOL shift;
} USCharMap[] = {
  { VK_SPACE, FALSE }, // 20 ' '
  { '1', TRUE }, // 21 '!'
  { VK_QUOTE, TRUE }, // 22 '"'
  { '3', TRUE }, // 23 '#'
  { '4', TRUE }, // 24 '$'
  { '5', TRUE }, // 25 '%'
  { '7', TRUE }, // 26 '&'
  { VK_QUOTE, FALSE }, // 27 '''
  { '9', TRUE }, // 28 '('
  { '0', TRUE }, // 29 ')'
  { '8', TRUE }, // 2A '*'
  { VK_EQUAL, TRUE }, // 2B '+'
  { VK_COMMA, FALSE }, // 2C ','
  { VK_HYPHEN, FALSE }, // 2D '-'
  { VK_PERIOD, FALSE }, // 2E '.'
  { VK_SLASH, FALSE }, // 2F '/'

  { '0', FALSE }, // 30 '0'
  { '1', FALSE }, // 31 '1'
  { '2', FALSE }, // 32 '2'
  { '3', FALSE }, // 33 '3'
  { '4', FALSE }, // 34 '4'
  { '5', FALSE }, // 35 '5'
  { '6', FALSE }, // 36 '6'
  { '7', FALSE }, // 37 '7'
  { '8', FALSE }, // 38 '8'
  { '9', FALSE }, // 39 '9'
  { VK_COLON, TRUE }, // 3A ':'
  { VK_COLON, FALSE }, // 3B ';'
  { VK_COMMA, TRUE }, // 3C '<'
  { VK_EQUAL, FALSE }, // 3D '='
  { VK_PERIOD, TRUE }, // 3E '>'
  { VK_SLASH, TRUE }, // 3F '?'

  { '2', TRUE }, // 40 '@'
  { 'A', TRUE }, // 41 'A'
  { 'B', TRUE }, // 42 'B'
  { 'C', TRUE }, // 43 'C'
  { 'D', TRUE }, // 44 'D'
  { 'E', TRUE }, // 45 'E'
  { 'F', TRUE }, // 46 'F'
  { 'G', TRUE }, // 47 'G'
  { 'H', TRUE }, // 48 'H'
  { 'I', TRUE }, // 49 'I'
  { 'J', TRUE }, // 4A 'J'
  { 'K', TRUE }, // 4B 'K'
  { 'L', TRUE }, // 4C 'L'
  { 'M', TRUE }, // 4D 'M'
  { 'N', TRUE }, // 4E 'N'
  { 'O', TRUE }, // 4F 'O'

  { 'P', TRUE }, // 50 'P'
  { 'Q', TRUE }, // 51 'Q'
  { 'R', TRUE }, // 52 'R'
  { 'S', TRUE }, // 53 'S'
  { 'T', TRUE }, // 54 'T'
  { 'U', TRUE }, // 55 'U'
  { 'V', TRUE }, // 56 'V'
  { 'W', TRUE }, // 57 'W'
  { 'X', TRUE }, // 58 'X'
  { 'Y', TRUE }, // 59 'Y'
  { 'Z', TRUE }, // 5A 'Z'
  { VK_LBRKT, FALSE }, // 5B '['
  { VK_BKSLASH, FALSE }, // 5C '\'
  { VK_RBRKT, FALSE }, // 5D ']'
  { '6', TRUE }, // 5E '^'
  { VK_HYPHEN, TRUE }, // 5F '_'

  { VK_ACCENT, FALSE }, // 60 '`'
  { 'A', FALSE }, // 61 'a'
  { 'B', FALSE }, // 62 'b'
  { 'C', FALSE }, // 63 'c'
  { 'D', FALSE }, // 64 'd'
  { 'E', FALSE }, // 65 'e'
  { 'F', FALSE }, // 66 'f'
  { 'G', FALSE }, // 67 'g'
  { 'H', FALSE }, // 68 'h'
  { 'I', FALSE }, // 69 'i'
  { 'J', FALSE }, // 6A 'j'
  { 'K', FALSE }, // 6B 'k'
  { 'L', FALSE }, // 6C 'l'
  { 'M', FALSE }, // 6D 'm'
  { 'N', FALSE }, // 6E 'n'
  { 'O', FALSE }, // 6F 'o'

  { 'P', FALSE }, // 70 'p'
  { 'Q', FALSE }, // 71 'q'
  { 'R', FALSE }, // 72 'r'
  { 'S', FALSE }, // 73 's'
  { 'T', FALSE }, // 74 't'
  { 'U', FALSE }, // 75 'u'
  { 'V', FALSE }, // 76 'v'
  { 'W', FALSE }, // 77 'w'
  { 'X', FALSE }, // 78 'x'
  { 'Y', FALSE }, // 79 'y'
  { 'Z', FALSE }, // 7A 'z'
  { VK_LBRKT, TRUE }, // 7B '{'
  { VK_BKSLASH, TRUE }, // 7C '|'
  { VK_RBRKT, TRUE }, // 7D '}'
  { VK_ACCENT, TRUE }  // 7E '~'
};


KMX_BOOL km::core::kmx::MapUSCharToVK(KMX_WORD ch, PKMX_WORD puKey, PKMX_DWORD puShiftFlags) {
  assert(puKey != NULL);
  assert(puShiftFlags != NULL);
  if (ch >= 0x20 && ch < 0x7F) {
    *puKey = USCharMap[ch - 0x20].key;
    *puShiftFlags = ISVIRTUALKEY | (USCharMap[ch - 0x20].shift ? K_SHIFTFLAG : 0);
    return TRUE;
  }
  return FALSE;
}
