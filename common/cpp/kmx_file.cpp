/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by mcdurdin on 2026-10-03
 *
 * Number of parameters for xstring codes
 */
#include "kmx_file.h"

#ifdef KM_CORE_LIBRARY
// TODO: move this to a common namespace keyman::common::kmx_file or similar in the future
namespace km {
namespace core {
namespace kmx {
#endif

/**
 * Define the number of parameters for each of the extended string codes:
 * * -1 means the code should not (or cannot) occur; treated as if the code is
 *   not present.
 *
 * * 0, 1, 2, 3 parameters otherwise, each one 16 bits (i.e. one u16 char) wide.
 *   Each parameter must be a non-zero value (0x0001-0xFFFF).
 *
 * * If a UC_SENTINEL is followed by any other code, the code is returned as a
 *   character.
 */
const int CODE__SIZE[] = {
    -1,  // undefined                0x00
    1,   // CODE_ANY                 0x01
    2,   // CODE_INDEX               0x02
    0,   // CODE_CONTEXT             0x03
    0,   // CODE_NUL                 0x04
    1,   // CODE_USE                 0x05
    0,   // CODE_RETURN              0x06
    0,   // CODE_BEEP                0x07
    1,   // CODE_DEADKEY             0x08
    -1,  // unused                   0x09
    2,   // CODE_EXTENDED            0x0A
    -1,  // CODE_EXTENDEDEND         0x0B (unused)
    1,   // CODE_SWITCH              0x0C (unused, marked as such in 19.0)
    -1,  // CODE_KEY                 0x0D (unused, marked as such in 19.0)
    0,   // CODE_CLEARCONTEXT        0x0E (deprecated in 19.0)
    1,   // CODE_CALL                0x0F
    -1,  // UC_SENTINEL_EXTENDEDEND  0x10 (not valid with UC_SENTINEL)
    1,   // CODE_CONTEXTEX           0x11
    1,   // CODE_NOTANY              0x12
    2,   // CODE_SETOPT              0x13
    3,   // CODE_IFOPT               0x14
    1,   // CODE_SAVEOPT             0x15
    1,   // CODE_RESETOPT            0x16
    3,   // CODE_IFSYSTEMSTORE       0x17
    2    // CODE_SETSYSTEMSTORE      0x18
};

// Ensure that all CODE_### sizes are defined
static_assert(sizeof(CODE__SIZE) / sizeof(CODE__SIZE[0]) == (CODE_LASTCODE + 1), "Size of array CODE__SIZE not correct");

#ifdef KM_CORE_LIBRARY
} // namespace kmx
} // namespace core
} // namespace km
#endif
