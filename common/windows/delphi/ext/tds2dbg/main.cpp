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

#include "tds2dbg.h"
#include <iostream>
#include <string>
#include <locale>

void printUsage(const char* argv0)
{
   std::cerr
      << "Usage: " << argv0
      << " [-h|--help] [-t|--types] [-s|--statics] [--] exefilenamewithextension\n"
         "Options:\n"
         " -h, --help     Prints this message\n"
         " -v, --verbose  Increase verbosity\n"
         " -q, --quiet    Operate silently\n"
         " -t, --types    Convert types (incomplete feature)\n"
         " -s, --statics  Treat statics as publics\n"
         " -b, --barecv   Write bare code view data\n"
      << std::endl;
}

int main(int argc, char* argv[])
{
   int          argNo            = 1;
   unsigned int verbosity        = 1;
   bool         convertTypes     = false;
   bool         staticsAsPublics = false;
   bool         bareCodeView     = false;

   while(argc > 1) {
      char* a = argv[argNo];
      if('-' != *a || 0==strcmp("--",a)) {
         break;
      } else if(0==strcmp("-h",a) || 0==strcmp("--help",a)) {
         printUsage(argv[0]);
         return 0;
      } else if(0==strcmp("--verbose",a) || 0==strcmp("-v",a)) {
         if(verbosity) ++verbosity;
      } else if(0==strcmp("--quiet",a) || 0==strcmp("-q",a)) {
         verbosity = 0;
      } else if(0==strcmp("--types",a) || 0==strcmp("-t",a)) {
         convertTypes = true;
      } else if(0==strcmp("--statics",a) || 0==strcmp("-s",a)) {
         staticsAsPublics = true;
      } else if(0==strcmp("--barecv",a) || 0==strcmp("-b",a)) {
         bareCodeView = true;
      } else {
         std::cerr << "Unknown option " << a << std::endl;
         printUsage(argv[0]);
         return 1;
      }
      --argc;
      ++argNo;
   }

   if(2 != argc) {
      printUsage(argv[0]);
      return 1;
   }

   std::string exeFile(argv[argNo]);
   std::string::size_type dotP = exeFile.rfind('.');
   if(std::string::npos == dotP) {
      std::cerr
         << "Argument exe file name must be specified with extension"
         << std::endl;
      return 2;
   }

   std::string tdsFile(exeFile.substr(0,dotP+1));
   std::string dbgFile(tdsFile);

   const std::ctype<char>& ctf =
      std::use_facet<std::ctype<char> >(std::locale());
   std::string::size_type ep = dotP;
   while(exeFile.length() != ep) {
      exeFile[ep] = ctf.tolower(exeFile[ep]);
      ++ep;
   }

   std::string ext(exeFile.substr(dotP+1));

   if(!ext.compare("tds") || !ext.compare("dbg")) {
      std::cerr
         << "Executable file cannot have extension .tds or .dbg"
         << std::endl;
      return 1;
   }

   tdsFile.append("tds");
   dbgFile.append("dbg");

   return tds2dbg(exeFile.c_str(),tdsFile.c_str(),dbgFile.c_str(),
                  verbosity,convertTypes,staticsAsPublics,bareCodeView);
}

// Local Variables:
// mode:c++
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
