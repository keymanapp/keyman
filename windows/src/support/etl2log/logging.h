#pragma once

#define __FILENAME__ (strrchr("\\" __FILE__, '\\') + 1)

#define DoLogError(message,...)   DoLog(2, __FILENAME__, __LINE__, (message), __VA_ARGS__)
#define DoLogWarning(message,...) if(DoVerboseLogging()) DoLog(1, __FILENAME__, __LINE__, (message), __VA_ARGS__)
#define DoLogInfo(message,...)    if(DoVerboseLogging()) DoLog(0, __FILENAME__, __LINE__, (message), __VA_ARGS__)
#define DoShowInfo(message,...)   DoLog(0, __FILENAME__, __LINE__, (message), __VA_ARGS__)

void DoLog(int level, const char *file, int line, const wchar_t *message, ...);
BOOL DoVerboseLogging();
inline BOOL LogToDebugConsole();
void hexStr2(PBYTE data, int len, wchar_t *out);
