#pragma once

#include <vector>

void msgproc(const KMCMP_COMPILER_RESULT_MESSAGE &message, void* context);
const std::vector<uint8_t> loadfileProc(const std::string& filename, const std::string& baseFilename);

extern std::vector<int> error_vec;