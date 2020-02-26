/*
  Copyright:        Copyright (C) 2003-2018 SIL International.
  Authors:          mcdurdin
*/

#pragma once

#include "kmx_base.h"

namespace km {
namespace kbp {
namespace kmx {

#define MAXACTIONQUEUE  1024

typedef struct
{
  int ItemType;
  KMX_DWORD dwData;
} KMX_Action;

// QueueAction ItemTypes
#define QIT_VKEYDOWN  0
#define QIT_VKEYUP    1
#define QIT_VSHIFTDOWN  2
#define QIT_VSHIFTUP  3
#define QIT_CHAR    4
#define QIT_DEADKEY   5
#define QIT_BELL    6
#define QIT_BACK    7
#define QIT_CAPSLOCK  8
#define QIT_INVALIDATECONTEXT 9

#define QIT_MAX     9

#define QVK_EXTENDED 0x00010000 // Flag for QIT_VKEYDOWN to indicate an extended key
#define QVK_KEYMASK  0x0000FFFF
#define QVK_FLAGMASK 0xFFFF0000

class KMX_Actions
{
private:
  KMX_Context *m_context;
  KMX_Action Queue[MAXACTIONQUEUE]; //TODO turn into List
  int QueueSize;

public:
  KMX_Actions(KMX_Context *context);

  /* Queue functions */

  void ResetQueue();
  KMX_BOOL IsQueueEmpty() { return QueueSize == 0; }
  KMX_BOOL QueueAction(int ItemType, KMX_DWORD dwData);
  int Length() { return QueueSize; }
  KMX_Action Get(int index) { return Queue[index]; }
};

} // namespace kmx
} // namespace kbp
} // namespace km

