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

/// @file log.h Log utilities

#ifndef LOG_H
#define LOG_H

#include <iostream>

/// @brief Logger class.
class Log
{
public:

   /// @brief Constructor.
   Log() : m_verbosity(0), m_nullStream(&m_nullStreamBuf) {}

   /// @brief Sets the level of verbosity
   ///
   /// @param verbosity New level of verbosity. 0 means silence.
   void setVerbosity(unsigned int verbosity) { m_verbosity = verbosity; }

   /// @brief Returns a reference to a stream handling the requested level
   ///        of verbosity.
   ///
   /// @param verbosity The requested level of verbosity.
   std::ostream& operator()(unsigned int verbosity = 0);

protected:

   /// @brief Helper class implementing a dummy stream buffer that
   ///        discards everything sent to it.
   ///
   /// @ingroup HMILoggingFeatures
   class NullStreamBuf : public std::streambuf
   {
   public:

      /// @brief Constructor
      NullStreamBuf();

   };

protected:

   /// @brief The current level of verbosity.
   unsigned int  m_verbosity;

   /// @brief Supporting stream buffer for m_nullStream.
   NullStreamBuf m_nullStreamBuf;

   /// @brief Internal stream used as a null-stream that discards
   ///        everything streamed to it.
   std::ostream  m_nullStream;

};

/// @brief Accesser for the global Log instance.
///
/// @return Reference to the global log.
Log& mylog();

#endif LOG_H

// Local Variables:
// mode:c++
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
