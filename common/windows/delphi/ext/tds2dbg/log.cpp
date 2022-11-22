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

/// @file log.cpp Log utilitites

#include "log.h"

std::ostream& Log::operator()(unsigned int verbosity)
{
   if(verbosity<m_verbosity) {
      return std::cout;
   } else {
      return m_nullStream;
   }
}

Log::NullStreamBuf::NullStreamBuf()
{
}

Log& mylog()
{
   static Log _log;
   return _log;
}


// Local Variables:
// mode:c++
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
