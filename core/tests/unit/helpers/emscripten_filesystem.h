#pragma once

#ifdef __EMSCRIPTEN__
#include <iostream>
#include "path.hpp"

bool get_wasm_file_path(const std::string& filename, km::core::path& result);

#endif