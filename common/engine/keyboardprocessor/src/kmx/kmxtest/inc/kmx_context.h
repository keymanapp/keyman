/*
  Copyright:        Copyright (C) 2003-2018 SIL International.
  Authors:          mcdurdin
*/

#pragma once

#include "kmx_base.h"

#define MAXCONTEXT 64

class AppContext
{
private:
	WCHAR CurContext[MAXCONTEXT];
	int pos;

public:
	AppContext();
  void CopyFrom(AppContext *source);
	void Add(WCHAR ch);
	void Delete();
	void Reset();
	void Get(WCHAR *buf, int bufsize);
	void Set(const WCHAR *buf);
	WCHAR *BufMax(int n);
	WCHAR *Buf(int n);
	BOOL CharIsDeadkey();
  BOOL CharIsSurrogatePair();
};
