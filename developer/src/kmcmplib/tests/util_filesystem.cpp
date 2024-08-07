
#ifdef __EMSCRIPTEN__
#include <emscripten.h>
#include <emscripten/bind.h>
#endif

#include <cassert>
#include <algorithm>
#include <iostream>
#include "util_filesystem.h"

#ifdef _MSC_VER
#include <io.h>
#else
#include <unistd.h>
#endif

//#define _DEBUG_FOPEN  1

#ifdef __EMSCRIPTEN__

const std::string get_wasm_file_path(const std::string& filename) {
  // Verify that we are passing a fully-qualified path
  // TODO: we need to support relative paths based on CWD
#if _DEBUG_FOPEN
  std::cout << "get_wasm_file_path ENTER (" << filename << ")" << std::endl;
#endif

  assert(
    (filename.length() > 0 && filename.at(0) == '/') ||
    (filename.length() > 1 && filename.at(1) == ':')
  );

#if _DEBUG_FOPEN
  std::cout << "get_wasm_file_path assert passed " << std::endl;
#endif

  EM_ASM_({
    //console.log('EM_ASM enter');
    let path = Module.UTF8ToString($0);
    const isWin32Path = path.match(/^[a-zA-Z]:/);

    //console.log('EM_ASM path = '+path);
    let root = '/nodefs-mount';
    if(!FS.analyzePath(root).exists) {
      //console.log('EM_ASM mkdir '+root);
      FS.mkdir(root);
      if(!isWin32Path) {
        //console.log('EM_ASM mount '+root);
        FS.mount(NODEFS, {root : '/'}, root);
      }
    }

    if(isWin32Path) {
      // Win32 path, one mount per drive
      root += "/" + path.charAt(0);
      if(!FS.analyzePath(root).exists) {
        //console.log('EM_ASM mkdir '+root);
        FS.mkdir(root);
        //console.log('EM_ASM mount '+root);
        FS.mount(NODEFS, {root : path.substr(0,3) }, root);
      }
    }
  }, filename.c_str());

#if _DEBUG_FOPEN
  std::cout << "get_wasm_file_path EM_ASM_ passed " << std::endl;
#endif

  std::string f = std::string("/nodefs-mount");

  if(filename.length() > 2 && filename.at(1) == ':') {
    f += std::string("/") + filename.at(0) + filename.substr(2);
  } else {
    f += filename;
  }

#if _DEBUG_FOPEN
  std::cout << "get_wasm_file_path opening virtual path: " << f << std::endl;
#endif

  return f;
}

FILE* fopen_wasm(const std::string& filename, const std::string& mode) {
  std::string f = get_wasm_file_path(filename);
  FILE* fp = fopen(f.c_str(), mode.c_str());

#if _DEBUG_FOPEN
  std::cout << "get_wasm_file_path fopen called, returned " << fp << std::endl;
#endif

  return fp;
}

#endif

#ifndef _MSC_VER
#ifdef __EMSCRIPTEN__
#define fopen_wrapper fopen_wasm
#else
#define fopen_wrapper fopen
#endif
#endif

FILE* Open_File(const KMX_CHAR* Filename, const KMX_CHAR* mode) {
#ifdef _MSC_VER
  std::string cpath = Filename; //, cmode = mode;
  std::replace(cpath.begin(), cpath.end(), '/', '\\');
  return fopen(cpath.c_str(), (const KMX_CHAR*) mode);
#else
  return fopen_wrapper(Filename, mode);
  std::string cpath, cmode;
  cpath = (const KMX_CHAR*) Filename;
  cmode = (const KMX_CHAR*) mode;
  return fopen_wrapper(cpath.c_str(), cmode.c_str());
#endif
};

FILE* Open_File(const KMX_WCHART* Filename, const KMX_WCHART* mode) {
#ifdef _MSC_VER
  std::wstring cpath = Filename; //, cmode = mode;
  std::replace(cpath.begin(), cpath.end(), '/', '\\');
  return _wfsopen(cpath.c_str(), mode, _SH_DENYWR);
#else
  std::string cpath, cmode;
  cpath = string_from_wstring(Filename);
  cmode = string_from_wstring(mode);
  return fopen_wrapper(cpath.c_str(), cmode.c_str());
#endif
};

FILE* Open_File(const KMX_WCHAR* Filename, const KMX_WCHAR* mode) {
#ifdef _MSC_VER
  std::wstring cpath = wstring_from_u16string(Filename);
  std::wstring cmode = wstring_from_u16string(mode);
  std::replace(cpath.begin(), cpath.end(), '/', '\\');
  return _wfsopen(cpath.c_str(), cmode.c_str(), _SH_DENYWR);
#else
  std::string cpath, cmode;
  cpath = string_from_u16string(Filename);
  cmode = string_from_u16string(mode);
  return fopen_wrapper(cpath.c_str(), cmode.c_str());
#endif
};

KMX_BOOL kmcmp_FileExists(const KMX_CHAR *filename) {

#ifdef _MSC_VER
  intptr_t n;
  _finddata_t fi;
  if((n = _findfirst(filename, &fi)) != -1) {
    _findclose(n);
    return TRUE;
  }
#else //!_MSC_VER

#ifdef __EMSCRIPTEN__
  std::string f = get_wasm_file_path(filename);
#else //!__EMSCRIPTEN__
  std::string f = filename;
#endif // !__EMSCRIPTEN__

  if(access(f.c_str(), F_OK) != -1) {
    return TRUE;
  }

#endif // !_MSC_VER

  return FALSE;
}

KMX_BOOL kmcmp_FileExists(const KMX_WCHAR* filename) {
#ifdef _MSC_VER
  intptr_t n;
  _wfinddata_t fi;
  if((n = _wfindfirst((wchar_t*) filename, &fi)) != -1) {
    _findclose(n);
    return TRUE;
  }
#else
  std::string cpath;
  cpath = string_from_u16string(filename);
  return kmcmp_FileExists(cpath.c_str());
#endif

  return FALSE;
};


bool IsRelativePath(KMX_CHAR const * p) {
  // Relative path (returns TRUE):
  //  ..\...\BITMAP.BMP
  //  PATH\BITMAP.BMP
  //  BITMAP.BMP

  // Semi-absolute path (returns FALSE):
  //  \...\BITMAP.BMP

  // Absolute path (returns FALSE):
  //  C:\...\BITMAP.BMP
  //  \\SERVER\SHARE\...\BITMAP.BMP

  if ((*p == '\\') || (*p == '/')) return FALSE;
  if (*p && *(p + 1) == ':') return FALSE;

  return TRUE;
}

bool IsRelativePath(KMX_WCHAR const * p) {
  // Relative path (returns TRUE):
  //  ..\...\BITMAP.BMP
  //  PATH\BITMAP.BMP
  //  BITMAP.BMP

  // Semi-absolute path (returns FALSE):
  //  \...\BITMAP.BMP

  // Absolute path (returns FALSE):
  //  C:\...\BITMAP.BMP
  //  \\SERVER\SHARE\...\BITMAP.BMP

  if ((*p == u'\\') || (*p == u'/'))return FALSE;
  if (*p && *(p + 1) == u':') return FALSE;

  return TRUE;
}