/*
  Copyright:    © 2011,2018 SIL International.
  Description:  UTF{8,16,32} codec template library, providing an iterator
                interface for easy encoding or decoding of Unicode USVs into
                UTF codeunits.
  Create Date:  2011
  Authors:      Tim Eves (TSE)
  History:      27 Sep 2018 - TSE - Imported from graphite2 project.
                25 Oct 2018 - TSE - Relicensed under the MIT license for
                                    inclusion in the Keyman project.
*/
#pragma once

#include <algorithm>
#include <cstdint>
#include <cstddef>
#include <cstdlib>
#include <string>

typedef uint32_t  uchar_t;

/* Intentional fallthrough */
#if defined(__clang__)
#define KMN_FALLTHROUGH [[clang::fallthrough]]
#elif defined(__GNUC__) && __GNUC__ >=  7
#define KMN_FALLTHROUGH [[gnu::fallthrough]]
#else
#define KMN_FALLTHROUGH ((void)0)
#endif

template <int N>
struct _utf_codec
{
    typedef uchar_t codeunit_t;

    static void     put(codeunit_t * cp, const uchar_t , int8_t & len) throw();
    static uchar_t  get(const codeunit_t * cp, int8_t & len) throw();
    static bool     validate(const codeunit_t * s, const codeunit_t * const e) throw();
};


template <>
struct _utf_codec<32>
{
private:
    static const uchar_t    limit = 0x110000;
public:
    typedef uint32_t  codeunit_t;

    inline
    static void put(codeunit_t * cp, const uchar_t usv, int8_t & l) throw()
    {
        *cp = usv; l = 1;
    }

    inline
    static uchar_t get(const codeunit_t * cp, int8_t & l) throw()
    {
        if (cp[0] < limit)  { l = 1;  return cp[0]; }
        else                { l = -1; return 0xFFFD; }
    }

    inline
    static bool validate(const codeunit_t * s, const codeunit_t * const e) throw()
    {
        return s <= e;
    }
};


template <>
struct _utf_codec<16>
{
private:
    static const int32_t  lead_offset      = 0xD800 - (0x10000 >> 10);
    static const int32_t  surrogate_offset = 0x10000 - (0xD800 << 10) - 0xDC00;
public:
    typedef uint16_t  codeunit_t;

    inline
    static void put(codeunit_t * cp, const uchar_t usv, int8_t & l) throw()
    {
        if (usv < 0x10000)  { l = 1; cp[0] = codeunit_t(usv); }
        else
        {
          l -= 2; if (l < 0) return;
          // We have space so continue.
          cp[0] = codeunit_t(lead_offset + (usv >> 10));
          cp[1] = codeunit_t(0xDC00 + (usv & 0x3FF));
          l = 2;
        }
    }

    inline
    static uchar_t get(const codeunit_t * cp, int8_t & l) throw()
    {
        const uint32_t    uh = cp[0];

        if (uh < 0xD800|| uh > 0xDFFF) { l = 1; return uh; }
        if (uh > 0xDBFF || l < 2) { l = -1; return 0xFFFD; }
        const uint32_t ul = cp[1];
        if (ul < 0xDC00 || ul > 0xDFFF) { l = -1; return 0xFFFD; }
        l = 2;
        return (uh<<10) + ul + surrogate_offset;
    }

    inline
    static bool validate(const codeunit_t * s, const codeunit_t * const e) throw()
    {
        const ptrdiff_t n = e-s;
        if (n <= 0) return n == 0;
        const uint32_t u = *(e-1); // Get the last codepoint
        return (u < 0xD800 || u > 0xDBFF);
    }
};


template <>
struct _utf_codec<8>
{
private:
    static constexpr const int8_t sz_lut[16] = {
      1,1,1,1,1,1,1,1,    // 1 byte
      0,0,0,0,            // trailing byte
      2,2,                // 2 bytes
      3,                  // 3 bytes
      4                   // 4 bytes
    };
    static constexpr const uint8_t mask_lut[5] = {0x7f, 0xff, 0x3f, 0x1f, 0x0f};
    static constexpr const uchar_t limit = 0x110000;

public:
    typedef uint8_t   codeunit_t;

    inline
    static void put(codeunit_t * cp, const uchar_t usv, int8_t & l) throw()
    {
        --l; if (usv < 0x80)     {l = 1; cp[0] = usv; return; }
        --l; if (usv < 0x0800)   {if (l < 0) return; l = 2; cp[0] = 0xC0 + (usv >> 6);  cp[1] = 0x80 + (usv & 0x3F); return; };
        --l; if (usv < 0x10000)  {if (l < 0) return; l = 3; cp[0] = 0xE0 + (usv >> 12); cp[1] = 0x80 + ((usv >> 6) & 0x3F);  cp[2] = 0x80 + (usv & 0x3F); return; }
        else                {if (--l < 0) return; l = 4; cp[0] = 0xF0 + (usv >> 18); cp[1] = 0x80 + ((usv >> 12) & 0x3F); cp[2] = 0x80 + ((usv >> 6) & 0x3F); cp[3] = 0x80 + (usv & 0x3F); return; };
    }

    inline
    static uchar_t get(const codeunit_t * cp, int8_t & l) throw()
    {
        const int8_t seq_sz = sz_lut[*cp >> 4];
        uchar_t u = *cp & mask_lut[seq_sz];
        if (l < seq_sz) { l = -l; return 0xFFFD; }

        auto rl = 1;
        bool toolong = false;
        switch(seq_sz) {
            case 4:     u <<= 6; u |= *++cp & 0x3F; if (*cp >> 6 != 2) break; ++rl; toolong  = (u < 0x10); KMN_FALLTHROUGH;
            case 3:     u <<= 6; u |= *++cp & 0x3F; if (*cp >> 6 != 2) break; ++rl; toolong |= (u < 0x20); KMN_FALLTHROUGH;
            case 2:     u <<= 6; u |= *++cp & 0x3F; if (*cp >> 6 != 2) break; ++rl; toolong |= (u < 0x80); KMN_FALLTHROUGH;
            case 1:     l = seq_sz; break;
            case 0:     l = -1; return 0xFFFD;
        }

        if (rl != seq_sz || toolong  || u >= limit)
        {
            l = -rl;
            return 0xFFFD;
        }
        return u;
    }

    inline
    static bool validate(const codeunit_t * s, const codeunit_t * const e) throw()
    {
        const ptrdiff_t n = e-s;
        if (n <= 0) return n == 0;
        s += (n-1);
        if (*s < 0x80) return true;
        if (*s >= 0xC0) return false;
        if (n == 1) return true;
        if (*--s < 0x80) return true;
        if (*s >= 0xE0) return false;
        if (n == 2 || *s >= 0xC0) return true;
        if (*--s < 0x80) return true;
        if (*s >= 0xF0) return false;
        return true;
    }

};


template <typename C>
class _utf_iterator
{
    typedef _utf_codec<sizeof(C)*8> codec;

    C             * cp;
    mutable int8_t  sl,
                    sz;

public:
    typedef C           codeunit_type;
    typedef uchar_t     value_type;
    typedef uchar_t   * pointer;

    class reference
    {
        const _utf_iterator & _i;

        reference(const _utf_iterator & i): _i(i) {}
    public:
        operator value_type () const throw ()                   { return codec::get(_i.cp, _i.sl = _i.sz); }
        reference & operator = (const value_type usv) throw()   { codec::put(_i.cp, usv, _i.sl = _i.sz); return *this; }

        friend class _utf_iterator;
    };


    _utf_iterator(const void * us=0)    : cp(reinterpret_cast<C *>(const_cast<void *>(us))), sl(1), sz(127) { }

    _utf_iterator   & operator ++ ()    { cp += abs(sl); return *this; }

    bool operator == (const _utf_iterator & rhs) const throw() { sz = (int8_t)std::min(ptrdiff_t(rhs.cp - cp),ptrdiff_t(127)); return 0 >= sz;
    }
    bool operator != (const _utf_iterator & rhs) const throw() { return !operator==(rhs); }

    reference   operator * () const throw() { return *this; }
    pointer     operator ->() const throw() { return &operator *(); }

    operator codeunit_type * () const throw() { return cp; }

    bool error() const throw()  { return sl < 1; }
    bool validate(const _utf_iterator & e)  { return codec::validate(cp, e.cp); }
};



template <typename C, C SENTINAL=0>
class _utf_sentinal_iterator : public _utf_iterator<C>
{
  typedef C           codeunit_type;
  typedef uchar_t     value_type;
  typedef uchar_t   * pointer;

  static constexpr const codeunit_type  sentinal_value=SENTINAL;
public:
  _utf_sentinal_iterator(const void * us=0): _utf_iterator<C>(us) {}

  operator bool () const noexcept {
    return *static_cast<codeunit_type *>(*this) != SENTINAL;
  }
};

template <typename C>
struct utf
{
    typedef _utf_codec<sizeof(C)*8> codec;
    typedef typename codec::codeunit_t codeunit_t;

    typedef _utf_iterator<C>        iterator;
    typedef _utf_iterator<const C>  const_iterator;

    inline
    static bool validate(codeunit_t * s, codeunit_t * e) throw() {
        return _utf_codec<sizeof(C)*8>::validate(s,e);
    }
};


typedef utf<uint32_t> utf32;
typedef utf<uint16_t> utf16;
typedef utf<uint8_t>  utf8;


template<typename F, typename T>
std::basic_string<T> convert(std::basic_string<F> const &src);

template<>
inline
std::basic_string<char> convert(std::basic_string<char> const &src) {
  return src;
}

template<typename F, typename T>
std::basic_string<T> convert(std::basic_string<F> const &src) {
  using utf_const_iter = typename utf<typename utf<F>::codeunit_t>::const_iterator;
  using codeunit_t = typename utf<T>::codeunit_t;
  auto r = std::basic_string<T>();
  codeunit_t buf[4];
  int8_t l = 4;
  for (auto i = utf_const_iter(src.data()),
            e = decltype(i)(src.data() + src.size()); i != e && l > 0; ++i, l = 4) {
      utf<T>::codec::put(buf, uchar_t(*i), l);
      r.append(reinterpret_cast<T *>(&buf[0]), l);
  }
  return r;
}

template<typename T>
auto & operator << (std::basic_ostream<T> &os, std::u16string const &p) {
  return std::operator << (os, convert<char16_t,T>(p));
}

template<typename T>
auto & operator << (std::basic_ostream<T> &os, std::u32string const &p) {
  return std::operator << (os, convert<char32_t,T>(p));
}

template<typename T>
auto & operator << (std::basic_ostream<T> &os, char16_t const *s) {
  return std::operator << (os, convert<char16_t,T>(s));
}

template<typename T>
auto & operator << (std::basic_ostream<T> &os, char32_t const *s) {
  return std::operator << (os, convert<char32_t,T>(s));
}
