#ifndef STDAFX_H
#define STDAFX_H

#pragma once

#include <windows.h>
#include "simpleassert.h"
#include "..\\Drv\\ScopedObjects.h"
#include <sstream>
#include <string>

#ifdef _DEBUG

#include <cstdio>

// only pass narrow literals for the 'string' parameter
#define DBG_TEXT(string, ...) \
	do \
	{ \
		C_ASSERT(sizeof(*string) == sizeof(char)); \
		fwprintf(stderr, L"MsgListerApp: " CONCAT(L, string), __VA_ARGS__); \
		fputc('\n', stderr); \
	} \
	while(0)

#else

#define DBG_TEXT(string, ...) ((void)0)

#endif

#endif
