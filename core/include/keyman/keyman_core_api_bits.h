/*
  Copyright:    © 2018 SIL International.
  Description:  Cross platform support macros to define API function
                declspec/attributes and how each supported compiler or OS
                allows us to specify them.
  Create Date:  18 Oct 2018
  Authors:      Tim Eves (TSE)
  History:      18 Oct 2018 - TSE - Imported from graphite2 project.
                 6 Oct 2018 - TSE - Move into keyman folder.

*/
#pragma once

// Define API function declspec/attributes and how each supported compiler or OS
// allows us to specify them.
#if defined __GNUC__
  #define _kmn_and ,
  #define _kmn_tag_fn(a)        __attribute__((a))
  #define _kmn_deprecated_flag  deprecated
  #define _kmn_export_flag      visibility("default")
  #define _kmn_import_flag      visibility("default")
  #define _kmn_static_flag      visibility("hidden")
  #define _kmn_unused(x)        UNUSED_ ## x __attribute__((__unused__))
#else
  #define _kmn_unused(x)        UNUSED_ ## x

#endif

#if defined _WIN32 || defined __CYGWIN__
  typedef wchar_t  const * km_core_path_name;
  #define _KM_CORE_PATH_SEPARATOR (L'\\')
  #define _KM_CORE_EXT_SEPARATOR (L'.')
  #if defined __GNUC__  // These three will be redefined for Windows
    #undef _kmn_export_flag
    #undef _kmn_import_flag
    #undef _kmn_static_flag
  #else  // How MSVC sepcifies function level attributes adn deprecation
    #define _kmn_and
    #define _kmn_tag_fn(a)       __declspec(a)
    #define _kmn_deprecated_flag deprecated
  #endif
  #define _kmn_export_flag     dllexport
  #define _kmn_import_flag     dllimport
  #define _kmn_static_flag
#else
  typedef char const * km_core_path_name;
  #define _KM_CORE_PATH_SEPARATOR ('/')
  #define _KM_CORE_EXT_SEPARATOR ('.')
#endif

#if defined KM_CORE_LIBRARY_STATIC
  #define KMN_API             _kmn_tag_fn(_kmn_static_flag)
  #define KMN_DEPRECATED_API  _kmn_tag_fn(_kmn_deprecated_flag _kmn_and _kmn_static_flag)
#elif defined KM_CORE_LIBRARY_EXPORTING
  #define KMN_API             _kmn_tag_fn(_kmn_export_flag)
  #define KMN_DEPRECATED_API  _kmn_tag_fn(_kmn_deprecated_flag _kmn_and _kmn_export_flag)
#else
  #define KMN_API             _kmn_tag_fn(_kmn_import_flag)
  #define KMN_DEPRECATED_API  _kmn_tag_fn(_kmn_deprecated_flag _kmn_and _kmn_import_flag)
#endif

#ifndef KM_CORE_LIBRARY
#define KM_CORE_LIBRARY
#endif
#ifndef USE_CHAR16_T
#define USE_CHAR16_T
#endif

#include <km_types.h>
