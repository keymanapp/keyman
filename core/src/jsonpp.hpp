/*
  Copyright:    © 2011,2018 SIL International.
  Description:  JSON pretty printer for dumping debug/diagnostic internal
                state information.
  Create Date:  15 Dec 2011
  Authors:      Tim Eves (TSE)
  History:      28 Sep 2018 - TSE - Imported from graphite2 project.
                            - TSE - Refactored to use C++ std::ostream.
                25 Oct 2018 - TSE - Relicensed under the MIT license for
                                    inclusion in the Keyman project.
*/
#pragma once

#include <cstdint>
#include <ostream>
#include <vector>

#include "utfcodec.hpp"

class json
{
    // Prevent copying
    json(const json &) = delete;
    json & operator = (const json &) = delete;

    typedef void (*_context_t)(json &);

    std::ostream& _stream;
    char          _contexts[128], // context stack
                * _context,       // current context (top of stack)
                * _flatten;       // if !0 points to context above which
                                  //  pretty printed output should occur.
    std::vector<void *>  _env;

    void context(const char current) throw();
    void indent(const int d=0) throw();
    void push_context(const char, const char) throw();
    void pop_context() throw();

public:
    class closer;

    using string = const char *;
    using number = double;
    enum class integer : std::intmax_t {};
    enum class integer_u : std::uintmax_t {};
    using boolean = bool;
    static const std::nullptr_t  null;

    void setenv(unsigned int index, void *val) { _env.reserve(index + 1); if (index >= _env.size()) _env.insert(_env.end(), _env.size() - index + 1, 0); _env[index] = val; }
    void *getenv(unsigned int index) const { return _env[index]; }
    const std::vector<void *> &getenvs() const { return _env; }

    static void flat(json &) throw();
    static void close(json &) throw();
    static void object(json &) throw();
    static void array(json &) throw();
    static void item(json &) throw();

    json(std::ostream& stream) throw();
    ~json() throw ();

    auto & stream() const throw();

    json & operator << (const char16_t*) throw();
    json & operator << (string) throw();
    json & operator << (number) throw();
    json & operator << (integer) throw();
    json & operator << (integer_u) throw();
    json & operator << (boolean) throw();
    json & operator << (std::nullptr_t) throw();
    json & operator << (_context_t) throw();

    operator bool() const throw();
    bool good() const throw();
    bool eof() const throw();
};

class json::closer
{
    // Prevent copying.
    closer(const closer &) = delete;
    closer & operator = (const closer &) = delete;

    json * const    _j;
public:
    closer(json * const j) : _j(j) {}
    ~closer() throw() { if (_j)  *_j << close; }
};

inline
json::json(std::ostream& s) throw()
: _stream(s), _context(_contexts), _flatten(0)
{
    if (_stream.good())
        _stream.flush();
}


inline
json::~json() throw ()
{
    while (_context > _contexts)    pop_context();
}

inline
auto & json::stream() const throw()     { return _stream; }


inline
json & json::operator << (json::_context_t ctxt) throw()
{
    ctxt(*this);
    return *this;
}

inline
json & operator << (json & j, std::string const & s) throw() { return j << json::string(s.c_str()); }

inline
json & operator << (json & j, std::string const * s) throw() { return j << json::string(s->c_str()); }

inline
json & operator << (json & j, std::u16string const & s) throw() { return j << json::string(convert<char16_t,char>(s).c_str()); }

inline
json & operator << (json & j, std::u16string const * s) throw() { return j << json::string(convert<char16_t,char>(*s).c_str()); }

inline
json & operator << (json & j, signed char d) throw()   { return j << json::integer(d); }

inline
json & operator << (json & j, unsigned char d) throw() { return j << json::integer_u(d); }

inline
json & operator << (json & j, short int d) throw()   { return j << json::integer(d); }

inline
json & operator << (json & j, unsigned short int d) throw() { return j << json::integer_u(d); }

inline
json & operator << (json & j, int d) throw()         { return j << json::integer(d); }

inline
json & operator << (json & j, unsigned int d) throw()       { return j << json::integer_u(d); }

inline
json & operator << (json & j, long int d) throw()         { return j << json::integer(d); }

inline
json & operator << (json & j, unsigned long int d) throw()       { return j << json::integer_u(d); }

inline
json & operator << (json & j, long long int d) throw()         { return j << json::integer(d); }

inline
json & operator << (json & j, unsigned long long int d) throw()       { return j << json::integer_u(d); }

inline
json::operator bool() const throw()     { return good(); }

inline
bool json::good() const throw()         { return _stream.good(); }

inline
bool json::eof() const throw()          { return _stream.eof(); }
