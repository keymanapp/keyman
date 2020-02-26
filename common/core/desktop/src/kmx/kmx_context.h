/*
  Copyright:        Copyright (C) 2003-2018 SIL International.
  Authors:          mcdurdin
*/

#pragma once

#include "kmx_base.h"

#define MAXCONTEXT 64

namespace km {
namespace kbp {
namespace kmx {

class KMX_Context
{
private:
  KMX_WCHAR CurContext[MAXCONTEXT];
  int pos;

public:
  KMX_Context();
  void CopyFrom(KMX_Context *source);
  void Add(KMX_WCHAR ch);
  void Delete();
  void Reset();
  void Get(KMX_WCHAR *buf, int bufsize);
  void Set(const KMX_WCHAR *buf);
  KMX_WCHAR *Buf(int n);
  KMX_BOOL CharIsDeadkey();
  KMX_BOOL CharIsSurrogatePair();
};

} // namespace kmx
} // namespace kbp
} // namespace km
