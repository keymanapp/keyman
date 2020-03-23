/*
  Copyright:        Copyright (C) 2003-2018 SIL International.
  Authors:          mcdurdin
*/
#include <kmx/kmx_processor.h>

using namespace km::kbp;
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
  case QIT_VKEYDOWN:
    break;

  case QIT_DEADKEY:
    m_context->Add(UC_SENTINEL);
    m_context->Add(CODE_DEADKEY);
    m_context->Add((KMX_WORD) dwData);
    break;

  case QIT_CHAR:
    m_context->Add((KMX_WORD) dwData);
    break;

  case QIT_BACK:
    if(dwData == BK_BACKSPACE)  // User pressed backspace so delete deadkeys
      while(m_context->CharIsDeadkey()) m_context->Delete();

    m_context->Delete();

    if(dwData == BK_BACKSPACE)  // User pressed backspace so delete deadkeys
      while(m_context->CharIsDeadkey()) m_context->Delete();
    break;
  }

  return result;
}
