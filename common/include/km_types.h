#pragma once
#include <stdint.h>

#include <stdint.h>

/*
#if defined(_WIN32) || defined(_WIN64)
#define snprintf _snprintf
#define vsnprintf _vsnprintf
#define strcasecmp _stricmp
#define strncasecmp _strnicmp
#endif
*/

#if defined(__LP64__) || defined(_LP64)
/* 64-bit, g++ */
#define KMX_REQUIRES_REALIGNMENT
#endif

#if defined(_WIN64) && !defined(USE_64)
/* 64-bit, Windows */
#define KMX_REQUIRES_REALIGNMENT
#endif

#if defined(__EMSCRIPTEN__)
// Emscripten/WASM. Emscripten even though it uses 32-bit (and not 64-bit
// pointers like the 64-bit architectures above) requires 32-bit alignment
// for pointers which we don't always have in the KMX data from file
// (see #12844).
#define KMX_REQUIRES_REALIGNMENT
#endif

typedef uint32_t   KMX_DWORD;
typedef int32_t    KMX_BOOL;
typedef uint8_t    KMX_BYTE;
typedef uint16_t   KMX_WORD;

#if defined(__cplusplus)
typedef char16_t   km_core_cu;
typedef char32_t   km_core_usv;
#else
typedef uint16_t   km_core_cu;          // code unit
typedef uint32_t   km_core_usv;         // Unicode Scalar Value
#endif

typedef km_core_cu  KMX_WCHAR;    // wc,   16-bit UNICODE character
typedef KMX_WCHAR* PKMX_WCHAR;

typedef char       KMX_CHAR;
typedef char*      PKMX_STR;
typedef KMX_CHAR*  PKMX_CHAR;

typedef uint32_t   KMX_UINT;

typedef KMX_BYTE*  PKMX_BYTE;
typedef KMX_WORD*  PKMX_WORD;
typedef KMX_DWORD* PKMX_DWORD;

#ifndef FALSE
#define FALSE               0
#endif

#ifndef TRUE
#define TRUE                1
#endif

// Macros and types to support char16_t vs wchar_t depending on project

#ifdef USE_CHAR16_T
#define lpuch(x) u ## x
typedef  km_core_cu KMX_UCHAR;
#else
#define lpuch(x) L ## x
typedef  wchar_t KMX_UCHAR;
#endif

typedef KMX_UCHAR* KMX_PUCHAR;

// Alignment

/*
  When we read .kmx files, they have no alignment guarantees, so we need to tell
  the compiler to generate unaligned-safe code for accesses to COMP_ structure
  members. Note we are assuming that COMP_KEYBOARD is aligned because it is
  always the start of the file, so will be at the start of any buffer which will
  automatically be aligned correctly.

  TODO: consider using c++11 alignas
*/
#if defined(__EMSCRIPTEN__) || defined (__GNUC__) || defined (__clang__)
typedef KMX_DWORD __attribute__((aligned(1))) KMX_DWORD_unaligned;
typedef KMX_BOOL  __attribute__((aligned(1))) KMX_BOOL_unaligned;
typedef KMX_WORD  __attribute__((aligned(1))) KMX_WORD_unaligned;
#elif defined(_MSC_VER)
typedef KMX_DWORD __declspec(align(1)) KMX_DWORD_unaligned;
typedef KMX_BOOL  __declspec(align(1)) KMX_BOOL_unaligned;
typedef KMX_WORD  __declspec(align(1)) KMX_WORD_unaligned;
#else
// TODO: consider other platforms
#define KMX_DWORD_unaligned KMX_DWORD
#define KMX_BOOL_unaligned  KMX_BOOL
#define KMX_WORD_unaligned  KMX_WORD
#endif
