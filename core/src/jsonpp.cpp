/*
  Copyright:    © 2011,2018 SIL International.
  Description:  JSON pretty printer for dumping debug/diagnostic data structure.
  Create Date:  15 Dec 2011
  Authors:      Tim Eves (TSE)
  History:      28 Sep 2018 - TSE - Imported from graphite2 project.
                            - TSE - Refactored to use C++ std::ostream.
                25 Oct 2018 - TSE - Relicensed under the MIT license for
                                    inclusion in the Keyman project.
*/
#include <cassert>
#include <iomanip>
#include <limits>

#include "jsonpp.hpp"
#include "utfcodec.hpp"

#if defined(_MSC_VER)
#define FORMAT_INTMAX "%lli"
#define FORMAT_UINTMAX "%llu"
#else
#define FORMAT_INTMAX "%ji"
#define FORMAT_UINTMAX "%ju"
#endif

namespace
{
    enum
    {
        seq = ',',
        obj='}', member=':', empty_obj='{',
        arr=']', empty_arr='['
    };
}

const std::nullptr_t json::null = nullptr;

inline
void json::context(const char current) throw()
{
    _stream << *_context;
    indent();
    *_context = current;
}


void json::indent(const int d) throw()
{
    if (*_context == member || (_flatten  && _flatten < _context))
        _stream.put(' ');
    else
        _stream << std::endl << std::setw(4*int(_context - _contexts + d)) << "";
}


inline
void json::push_context(const char prefix, const char suffix) throw()
{
    assert(_context - _contexts < ptrdiff_t(sizeof _contexts));

    if (_context == _contexts)
        *_context = suffix;
    else
        context(suffix);
    *++_context = prefix;
}


void json::pop_context() throw()
{
    assert(_context > _contexts);

    if (*_context == seq)   indent(-1);
    else                    _stream.put(*_context);

    _stream.put(*--_context);
    if (_context == _contexts)  _stream << std::endl;
    _stream.flush();

    if (_flatten >= _context)   _flatten = 0;
    *_context = seq;
}


// These four functions cannot be inlined as pointers to these
// functions are needed for operator << (_context_t) to work.
void json::flat(json & j) throw()   { if (!j._flatten)  j._flatten = j._context; }
void json::close(json & j) throw()  { j.pop_context(); }
void json::object(json & j) throw() { j.push_context('{', '}'); }
void json::array(json & j) throw()  { j.push_context('[', ']'); }
void json::item(json & j) throw()
{
    while (j._context > j._contexts+1 && j._context[-1] != arr)
        j.pop_context();
}


json & json::operator << (json::string s) throw()
{
    const char ctxt = _context[-1] == obj ? *_context == member ? seq : member : seq;
    context(ctxt);
    _stream << '"' << s << '"';
    if (ctxt == member) _stream.put(' ');

    return *this;
}

json & json::operator <<(const char16_t* s) throw()
{
  std::u16string str16(s);
  std::string str = convert<char16_t, char>(str16);

  (*this) << str;
  return *this;
}

json & json::operator << (json::number f) throw()
{
    context(seq);
    if (std::numeric_limits<json::number>::infinity() == f)
        _stream << "Infinity";
    else if (-std::numeric_limits<json::number>::infinity() == f)
        _stream << "-Infinity";
    else if (std::numeric_limits<json::number>::quiet_NaN() == f ||
            std::numeric_limits<json::number>::signaling_NaN() == f)
          _stream << "NaN";
    else
        _stream << f;
    return *this;
}
json & json::operator << (json::integer d) throw()  { context(seq); _stream << intmax_t(d); return *this; }
json & json::operator << (json::integer_u d) throw(){ context(seq); _stream << uintmax_t(d); return *this; }
json & json::operator << (json::boolean b) throw()  { context(seq); _stream << (b ? "true" : "false"); return *this; }
json & json::operator << (std::nullptr_t)  throw()  { context(seq); _stream << "null"; return *this; }
