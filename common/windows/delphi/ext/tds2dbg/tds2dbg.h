// Copyright (c) 2009-2010, Bjarne Juul Pasgaard
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice, this permission notice and the following disclaimer
// appear in all copies.
//
// THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
// WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
// MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
// ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
// WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
// ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
// OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

#ifndef TDS2DBGH
#define TDS2DBGH

/// @brief Converts an exe and a tds file to a dbg file.
///
/// @param exeFileName      Name of EXE file.
/// @param tdsFileName      Name of TDS file.
/// @param dbgFileName      Name of DBG file.
/// @param verbosity        Verbosity level. 0 means silence.
/// @param doConvTypes      Should types be converted?
/// @param staticsAsPublics Treat statics as publics?
/// @param bareCodeView     Write bare code view (no header).
///
/// @todo doConvTypes == true is incomplete.
int tds2dbg(const char*  exeFileName,
            const char*  tdsFileName,
            const char*  dbgFileName,
            unsigned int verbosity,
            bool         doConvTypes,
	    bool         staticsAsPublics,
            bool         bareCodeView);

#endif
