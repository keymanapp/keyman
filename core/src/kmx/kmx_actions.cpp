/*
  Copyright:        Copyright (C) 2003-2018 SIL International.
  Authors:          mcdurdin
*/
#include <kmx/kmx_processevent.h>

using namespace km::core;
using namespace kmx;

void KMX_Actions::ResetQueue()
{
  QueueSize = 0;
}

KMX_Actions::KMX_Actions(KMX_Context *context)
{
  m_context = context;
  ResetQueue();
}

KMX_BOOL KMX_Actions::QueueAction(int ItemType, KMX_DWORD dwData)
{
  if (QueueSize > MAXACTIONQUEUE - 1)
  {
    DebugLog("App::QueueAction: queue size exceeded");
    return FALSE;
  }

  Queue[QueueSize].ItemType = ItemType;
  Queue[QueueSize].dwData = dwData;

  QueueSize++;

  int result = TRUE;

  switch(ItemType)
  {
  case QIT_DEADKEY:
    m_context->Add(UC_SENTINEL);
    m_context->Add(CODE_DEADKEY);
    m_context->Add((KMX_WORD) dwData);
    break;

  case QIT_CHAR:
    {
      char16_single buf;
      int len = Utf32CharToUtf16(dwData, buf);
      for(int i=0; i<len; i++) {
        m_context->Add(buf.ch[i]);
      }
    }
    break;

  case QIT_BACK:
    m_context->Delete();
    break;
  }

  return result;
}
