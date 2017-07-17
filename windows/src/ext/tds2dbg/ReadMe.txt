NAME

  tds2dbg - Borland TDS debug symbol file to Microsft DBG debug symbol file
            converter.

SYNOPSIS

  tds2dbg [ [-h|--help] | [-t|--types] [-s|--statics] [--] program.exe ]

DESCRIPTION

  tds2dbg will read the Borland debug symbol file associated with the
  Microsoft Windows executable program.exe (program.tds) and create a
  corresponding Microsoft DBG debug symbol file (program.dbg). Additionally,
  it will modify a flag in program.exe to signal that debug symbols are 
  available externally to the executable.

  The dbg files can be used with, say, windbg available from the
  Microsoft Debugging Tools at the Microsoft Windows Hardware Developer
  Central (http://www.microsoft.com/whdc).

OPTIONS

  -h, --help     Prints a help text. Running tds2dbg without arguments
                 will do the same.
  -v, --verbose  Each occurence of this option increases verbosity.
  -q, --quiet    Operate silently.
  -t, --types    Convert types (incomplete feature).
  -s, --statics  Treat statics as publics.
  -b, --barecv   Write bare code view data. No PE header is added. 

KNOWN ISSUES

  tds2dbg is currently only able to insert source code references, public
  symbols, and static symbols into the generated DBG file. Debug information
  on local variables is partially generated, but no debugger seems to be
  able to use this information. Additionally, conversion of types is not
  fully supported and is only done on request (the -t option).

VERSION

  This is the documentation for tds2dbg version 0.2.0

AUTHOR

   Copyright (C) 2009-2010, Bjarne Juul Pasgaard

LICENSE

   Copyright (C) 2009-2010, Bjarne Juul Pasgaard

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice, this permission notice and the following disclaimer
   appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

