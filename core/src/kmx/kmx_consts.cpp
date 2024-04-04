
#include "kmx_processevent.h"

namespace km {
namespace core {
namespace kmx {

const struct char_to_vkey s_char_to_vkey[] = {
  {KM_CORE_VKEY_SPACE, 0, 0},     //
  {'1', 1, 0},       // !
  {KM_CORE_VKEY_QUOTE, 1, 0},  // "
  {'3', 1, 0},       // #
  {'4', 1, 0},       // $
  {'5', 1, 0},       // %
  {'7', 1, 0},       // &
  {KM_CORE_VKEY_QUOTE, 0, 0},     // '
  {'9', 1, 0},       // (
  {'0', 1, 0},       // )
  {'8', 1, 0},       // *
  {KM_CORE_VKEY_EQUAL, 1, 0},  // +
  {KM_CORE_VKEY_COMMA, 0, 0},     // ,
  {KM_CORE_VKEY_HYPHEN, 0, 0},    // -
  {KM_CORE_VKEY_PERIOD, 0, 0},    // .
  {KM_CORE_VKEY_SLASH, 0, 0},     // /
  {'0', 0, 0},
  {'1', 0, 0},
  {'2', 0, 0},
  {'3', 0, 0},
  {'4', 0, 0},
  {'5', 0, 0},
  {'6', 0, 0},
  {'7', 0, 0},
  {'8', 0, 0},
  {'9', 0, 0},
  {KM_CORE_VKEY_COLON, 1, 0},  // :
  {KM_CORE_VKEY_COLON, 0, 0},     // ;
  {KM_CORE_VKEY_COMMA, 1, 0},  // <
  {KM_CORE_VKEY_EQUAL, 0, 0},     // =
  {KM_CORE_VKEY_PERIOD, 1, 0}, // >
  {KM_CORE_VKEY_SLASH, 1, 0},  // ?
  {'2', 1, 0},       // @
  {'A', 1, 1},
  {'B', 1, 1},
  {'C', 1, 1},
  {'D', 1, 1},
  {'E', 1, 1},
  {'F', 1, 1},
  {'G', 1, 1},
  {'H', 1, 1},
  {'I', 1, 1},
  {'J', 1, 1},
  {'K', 1, 1},
  {'L', 1, 1},
  {'M', 1, 1},
  {'N', 1, 1},
  {'O', 1, 1},
  {'P', 1, 1},
  {'Q', 1, 1},
  {'R', 1, 1},
  {'S', 1, 1},
  {'T', 1, 1},
  {'U', 1, 1},
  {'V', 1, 1},
  {'W', 1, 1},
  {'X', 1, 1},
  {'Y', 1, 1},
  {'Z', 1, 1},
  {KM_CORE_VKEY_LBRKT, 0, 0},
  {KM_CORE_VKEY_BKSLASH, 0, 0},
  {KM_CORE_VKEY_RBRKT, 0, 0},
  {'6', 1, 0},
  {KM_CORE_VKEY_HYPHEN, 1, 0},
  {KM_CORE_VKEY_BKQUOTE, 0, 0},
  {'A', 0, 1},
  {'B', 0, 1},
  {'C', 0, 1},
  {'D', 0, 1},
  {'E', 0, 1},
  {'F', 0, 1},
  {'G', 0, 1},
  {'H', 0, 1},
  {'I', 0, 1},
  {'J', 0, 1},
  {'K', 0, 1},
  {'L', 0, 1},
  {'M', 0, 1},
  {'N', 0, 1},
  {'O', 0, 1},
  {'P', 0, 1},
  {'Q', 0, 1},
  {'R', 0, 1},
  {'S', 0, 1},
  {'T', 0, 1},
  {'U', 0, 1},
  {'V', 0, 1},
  {'W', 0, 1},
  {'X', 0, 1},
  {'Y', 0, 1},
  {'Z', 0, 1},
  {KM_CORE_VKEY_LBRKT, 1, 0},
  {KM_CORE_VKEY_BKSLASH, 1, 0},
  {KM_CORE_VKEY_RBRKT, 1, 0},
  {KM_CORE_VKEY_BKQUOTE, 1, 0},
  {0, 0, 0}
};

} // namespace kmx
} // namespace core
} // namespace km
