/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Mnemonic layout support for linux
 */

#ifndef MCOMPILE_H
#define MCOMPILE_H
#include <vector>
#include <gdk/gdk.h>
#include "mc_kmxfile.h"

/** @brief convert mnemonic keyboard layout to positional keyboard layout and translate keyboard */
KMX_BOOL KMX_DoConvert(LPKMX_KEYBOARD kbd, KMX_BOOL bDeadkeyConversion, gint argc, gchar* argv[]);

#endif /*MCOMPILE_H*/
