#pragma once

#include <vector>

int msgproc(int line, uint32_t dwMsgCode, const char* szText, void* context);
bool loadfileProc(const char* filename, const char* baseFilename, void* data, int* size, void* context);

extern std::vector<int> error_vec;