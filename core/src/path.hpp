/*
  Copyright:    © 2018 SIL International.
  Description:  Internal keyboard class and adaptor class for the API.
  Create Date:  2 Oct 2018
  Authors:      Tim Eves (TSE)
  History:      2 Oct 2018 - TSE - Refactored out of km_core_keyboard_api.cpp
*/

#pragma once

#include <algorithm>
#include <string>
#include <type_traits>

#include "keyman_core.h"

#include "jsonpp.hpp"
#include "utfcodec.hpp"

// Forward declarations

namespace km {
namespace core
{
  class path
  {
  public:
    using char_type = std::remove_const_t<
                        std::remove_pointer_t<km_core_path_name>>;
    using string_type = std::basic_string<char_type>;
    static constexpr char_type const parent_separator = _KM_CORE_PATH_SEPARATOR,
                                     suffix_separator = _KM_CORE_EXT_SEPARATOR;
  private:
    string_type _path;

    void normalise() {
      #if '/' != _KM_CORE_PATH_SEPARATOR
      std::replace(_path.begin(), _path.end(), char_type('/'), _KM_CORE_PATH_SEPARATOR);
      #endif
    }

  public:
    template<class... Args>
    static path join(path const &start, Args&&... args) {
      auto r = start;
      for (path p: {args...})
        r._path.append(1, path::parent_separator).append(p);
      return r;
    }

    path() = default;
    path(path const &) = default;
    path(path &&)  = default;
    path & operator = (path const &) = default;
    path & operator = (path &&) = default;

    path(std::string const & p):    _path(convert<char, char_type>(p)) { normalise(); }
    path(std::u16string const & p): _path(convert<char16_t, char_type>(p)) { normalise(); }
    path(std::wstring const & p):   _path(convert<wchar_t, char_type>(p)) { normalise(); }
    template<typename C>
    path(C const * p): path(std::basic_string<C>(p)) {}

    /**
     * @return the enclosing directory's path
    */
    path parent() const {
      auto i = _path.find_last_of(parent_separator);
      return _path.substr(0, i == string_type::npos ? 0UL : i);
    }

    /**
     * @return just the filename, such as "angkor.kmx"
    */
    path name() const {
      auto i = _path.find_last_of(parent_separator);
      return _path.substr(i == string_type::npos ? 0UL : i+1);
    }

    /**
     * @return the suffix of the file, such as ".kmx"
    */
    path suffix() const {
      auto i = _path.find_last_of(suffix_separator);
      return _path.substr(i == string_type::npos ? _path.size() : i);
    }

    /**
     * @return just the child filename, such as "angkor" for "angkor.kmx"
    */
    path stem() const {
      auto psep = _path.find_last_of(parent_separator),
           ssep = _path.find_last_of(suffix_separator);
      return _path.substr(psep == string_type::npos ? 0UL : psep+1,
                          ssep == string_type::npos ? _path.size() : ssep);
    }

    /**
     * Mutate the path to change the extension
     * @param replacement new suffix, such as ".kmp". default is to remove the suffix
    */
    void replace_extension(path const & replacment = path()) {
      _path.resize(std::min(_path.find_last_of(suffix_separator), _path.size()));
      _path += replacment;
    }

    // template<typename T>
    // bool operator == (T const * rhs) const { return _path == rhs; }

    bool operator == (path const &rhs) const { return _path == rhs._path; }

    // template<typename T>
    // bool operator != (T const * rhs) const { return _path != rhs; }

    bool operator != (path const &rhs) const { return _path != rhs._path; }

    string_type const & native() const noexcept { return _path; }

    operator std::wstring () const { return convert<char_type,wchar_t>(_path); }
    operator std::string() const { return convert<char_type,char>(_path); };
    operator std::u16string() const {return convert<char_type,char16_t>(_path); };

    char_type const * c_str() const { return _path.c_str(); }

    path & operator += (path const & rhs) {
      _path += rhs._path; return *this;
    }

    path operator + (path const & rhs) const {
      auto r = *this; return r += rhs;
    }

    path & operator /= (path const & rhs) {
      _path.append(1,path::parent_separator).append(rhs._path);
      return *this;
    }

    path  operator / (path const & rhs) const {
      auto r = *this; return r /= rhs;
    }

    friend json & operator << (json &, path const &);
  };

  template<typename T>
  bool operator == (T const * lhs, path const & rhs) { return rhs == lhs; }

  template<typename T>
  bool operator != (T const * lhs, path const & rhs) { return rhs != lhs; }

  inline
  json & operator << (json &j, path const &p) {
    return j << static_cast<std::string>(p);
  }

  template<typename C>
  auto & operator << (std::basic_ostream<C> &os, path const &p) {
    os << static_cast<std::basic_string<C>>(p);
    return os;
  }

} // namespace core
} // namespace km
