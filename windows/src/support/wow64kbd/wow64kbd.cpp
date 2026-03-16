#include <windows.h>
#include <tchar.h>

void hexDump(const void* addr, const int len, int perLine);

typedef PBYTE(WINAPI* PKBDLAYERDESCRIPTORFUNC)(VOID);

int main(void) {
  BOOL bIsWow64;
  if (!IsWow64Process(GetCurrentProcess(), &bIsWow64)) {
    _tprintf(TEXT("IsWow64Process failed with %d\n"), GetLastError());
  } else {
    _tprintf(TEXT("IsWow64Process returned %d.\n"), bIsWow64);
  }

  USHORT processMachine, nativeMachine;
  if (!IsWow64Process2(GetCurrentProcess(), &processMachine, &nativeMachine)) {
    _tprintf(TEXT("Failed to call IsWow64Process2 with err=%d\n"), GetLastError());
  } else {
    _tprintf(TEXT("IsWow64Process2: processMachine=%x nativeMachine=%x\n"), processMachine, nativeMachine);
  }

  // For this test, we'll just load kbdus.dll from system32

  HMODULE hKbdLibrary = LoadLibrary(TEXT("kbdus.dll"));
  if (!hKbdLibrary) {
    _tprintf(TEXT("Failed to load kbdus.dll err=%d\n"), GetLastError());
    return 1;
  }

  PKBDLAYERDESCRIPTORFUNC KbdLayerDescriptorFunc =
      (PKBDLAYERDESCRIPTORFUNC)GetProcAddress(hKbdLibrary, "KbdLayerDescriptor");

  if (!KbdLayerDescriptorFunc) {
    _tprintf(TEXT("Failed to get KbdLayerDescriptor err=%d\n"), GetLastError());
    return 1;
  }

  const PBYTE data = (*KbdLayerDescriptorFunc)();
  if (!data) {
    _tprintf(TEXT("Failed to get data from KbdLayerDescriptor err=%d\n"), GetLastError());
    return 1;
  }

  _tprintf(TEXT("Address of data: %p\n"), data);
  // print hex dump of first 44 bytes, which is 32 bit kbdtables size
  hexDump(data, 44, 16);

  return 0;
}


// https://stackoverflow.com/a/7776146/1836776
//
// Usage:
//     hexDump(addr, len, perLine);
//         addr:    the address to start dumping from.
//         len:     the number of bytes to dump.
//         perLine: number of bytes on each output line.

void hexDump(const void* addr, const int len, int perLine) {
  // Silently ignore silly per-line values.

  if (perLine < 4 || perLine > 64)
    perLine = 16;

  int i;
  unsigned char buff[64 + 1];
  const unsigned char* pc = (const unsigned char*)addr;

  // Length checks.

  if (len == 0) {
    _tprintf(TEXT("  ZERO LENGTH\n"));
    return;
  }
  if (len < 0) {
    _tprintf(TEXT("  NEGATIVE LENGTH: %d\n"), len);
    return;
  }

  // Process every byte in the data.

  for (i = 0; i < len; i++) {
    // Multiple of perLine means new or first line (with line offset).

    if ((i % perLine) == 0) {
      // Only print previous-line ASCII buffer for lines beyond first.

      if (i != 0)
        _tprintf(TEXT("  %hs\n"), buff);

      // Output the offset of current line.

      _tprintf(TEXT("  %04x "), i);
    }

    // Now the hex code for the specific character.

    _tprintf(TEXT(" %02x"), pc[i]);

    // And buffer a printable ASCII character for later.

    if ((pc[i] < 0x20) || (pc[i] > 0x7e))  // isprint() may be better.
      buff[i % perLine] = '.';
    else
      buff[i % perLine] = pc[i];
    buff[(i % perLine) + 1] = '\0';
  }

  // Pad out last line if not exactly perLine characters.

  while ((i % perLine) != 0) {
    _tprintf(TEXT("   "));
    i++;
  }

  // And print the final ASCII buffer.

  _tprintf(TEXT("  %hs\n"), buff);
}
