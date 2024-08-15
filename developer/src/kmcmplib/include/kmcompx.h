#pragma once
#include <km_types.h>

// TODO: merge with kmcmplib.h

// TODO: these defines are copied out of unicode.h, because unicode.h still
//       has windows-specific types. These should be remerged at a future date

#define Uni_IsSurrogate1(ch) ((ch) >= 0xD800 && (ch) <= 0xDBFF)
#define Uni_IsSurrogate2(ch) ((ch) >= 0xDC00 && (ch) <= 0xDFFF)
#define Uni_IsSMP(ch) ((ch) >= 0x10000)

#define Uni_SurrogateToUTF32(ch, cl) (((ch) - 0xD800) * 0x400 + ((cl) - 0xDC00) + 0x10000)

#define Uni_UTF32ToSurrogate1(ch)	(((ch) - 0x10000) / 0x400 + 0xD800)
#define Uni_UTF32ToSurrogate2(ch)	(((ch) - 0x10000) % 0x400 + 0xDC00)

#define Uni_UTF16ToUTF32(p) (Uni_IsSurrogate1(*(p)) && Uni_IsSurrogate2(*((p)+1)) ? Uni_SurrogateToUTF32(*(p), *((p)+1)) : *(p))

//
// C++17 definition of Windows _countof
// TODO: remove use of _countof throughout
//

#ifndef _MSC_VER
#include <type_traits>
#include <cstddef>

template < typename T, size_t N >
size_t _countof( T ( & /*arr*/ )[ N ] )
{
    return std::extent< T[ N ] >::value;
}

#endif

// For MSVC, we want to use the std CRT library functions
// and ignore security warnings at this time, so we are
// not forced to use the non-portable _s variants.
#define _CRT_SECURE_NO_DEPRECATE

// strtok_s on MSVC works the same as strtok_r
#ifdef _MSC_VER
#define strtok_r strtok_s
#endif
