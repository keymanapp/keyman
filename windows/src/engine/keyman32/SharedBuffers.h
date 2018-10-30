#pragma once

#define MAX_SELECTKEYBOARDTSF_CIRCULARBUFFER_SIZE 16
#define GLOBAL_SHAREDBUFFER_FILE_MAPPING_NAME "KeymanEngine_SharedBuffer"

struct SelectKeyboardBuffer {
  WORD LangID;
  CLSID CLSID;
  GUID GUIDProfile;
};

struct SharedBuffer {
  SelectKeyboardBuffer SelectKeyboardBuffer[MAX_SELECTKEYBOARDTSF_CIRCULARBUFFER_SIZE];
};

class ISharedBufferManager {
public:
  virtual ~ISharedBufferManager() {}
  virtual BOOL ReadSelectKeyboardBuffer(DWORD dwIdentity, SelectKeyboardBuffer *skb) = 0;
};

ISharedBufferManager *GetThreadSharedBufferManager();
void CloseThreadSharedBufferManager();
