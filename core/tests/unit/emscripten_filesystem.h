#pragma once

#ifdef __EMSCRIPTEN__
#include <iostream>

const std::string get_wasm_file_path(const std::string& filename);

#endif