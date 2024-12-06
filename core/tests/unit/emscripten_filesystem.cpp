

#ifdef __EMSCRIPTEN__

#include <emscripten.h>
#include <iostream>
#include <test_assert.h>

const std::string get_wasm_file_path(const std::string& filename) {
  // Verify that we are passing a fully-qualified path
  // TODO: we need to support relative paths based on CWD
#if _DEBUG_FOPEN
  std::cout << "get_wasm_file_path ENTER (" << filename << ")" << std::endl;
#endif

  test_assert(
    (filename.length() > 0 && filename.at(0) == '/') ||
    (filename.length() > 1 && filename.at(1) == ':')
  );

#if _DEBUG_FOPEN
  std::cout << "get_wasm_file_path test_assert passed " << std::endl;
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

#endif
