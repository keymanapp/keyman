
#include "pch.h"
#include "SharedBuffers.h"

// TODO: Move serialiedkeyevent shared memory into this class (not event or mutex though, just the data)

class SharedBufferManager: public ISharedBufferManager {
private:
  HANDLE m_hMMF;
  SharedBuffer *m_pSharedData;

public:
  SharedBufferManager() {
    m_pSharedData = NULL;
    m_hMMF = OpenFileMapping(FILE_MAP_READ, FALSE, GLOBAL_SHAREDBUFFER_FILE_MAPPING_NAME);
    if (!m_hMMF) {
      DebugLastError("OpenFileMapping");
      return;
    }

    m_pSharedData = (SharedBuffer *)MapViewOfFile(m_hMMF, FILE_MAP_READ, 0, 0, sizeof(SharedBuffer));
    if (!m_pSharedData) {
      DebugLastError("MapViewOfFile");
      return;
    }
  }

  virtual ~SharedBufferManager() {
    if (m_pSharedData != NULL && !UnmapViewOfFile((LPCVOID)m_pSharedData)) {
      DebugLastError("CloseHandle(m_pSharedData)");
    }

    if (m_hMMF != NULL && !CloseHandle(m_hMMF)) {
      DebugLastError("CloseHandle(m_hMMF)");
    }
  }

  virtual BOOL ReadSelectKeyboardBuffer(DWORD dwIdentity, SelectKeyboardBuffer *skb) {
    if (dwIdentity >= MAX_SELECTKEYBOARDTSF_CIRCULARBUFFER_SIZE) {
      SendDebugMessageFormat("dwIdentity too large: %d", dwIdentity);
      return FALSE;
    }
    *skb = m_pSharedData->SelectKeyboardBuffer[dwIdentity];
    return TRUE;
  }
};

ISharedBufferManager *GetThreadSharedBufferManager() {
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if (!_td) {
    return NULL;
  }
  if (!_td->pSharedBufferManager) {
    _td->pSharedBufferManager = new SharedBufferManager();
  }
  return _td->pSharedBufferManager;
}

void CloseThreadSharedBufferManager() {
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if (!_td) {
    return;
  }
  if (_td->pSharedBufferManager) {
    delete _td->pSharedBufferManager;
    _td->pSharedBufferManager = NULL;
  }
}
