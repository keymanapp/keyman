#ifndef DBG_TEXT_H
#define DBG_TEXT_H

#pragma once

#include "stdafx.h"

// we can't print out unicode text at APC_LEVEL.
// Docs say you can only use the unicode format
// specifiers at PASSIVE_LEVEL so we convert to multibyte and print that
// (implemented in globals.cpp)
void PrintUnicode(PCUNICODE_STRING pusText);
void PrintUnicode(PCWSTR wszText, ULONG dwTextByteLen = 0);

// remember to
// ed nt!Kd_IHVDRIVER_mask 0xf
// in WinDbg to see dbg output

// macro expanding concat
#define CONCAT_IMP(x, y) x##y
#define CONCAT(x, y) CONCAT_IMP(x, y)

#define INT_ERROR_TEXT_NO_NEWLINE(...) \
	DbgPrintEx(DPFLTR_IHVDRIVER_ID, DPFLTR_ERROR_LEVEL, "MsgHookLister ("__VA_ARGS__);

#define ERROR_TEXT(...) \
	do \
	{ \
		INT_ERROR_TEXT_NO_NEWLINE(__FUNCTION__ "): " __VA_ARGS__); \
		DbgPrintEx(DPFLTR_IHVDRIVER_ID, DPFLTR_ERROR_LEVEL, "\n"); \
	} \
	while(0)

#define INT_ERROR_TEXT_U(dbgParams, uniText) \
	do \
	{ \
		INT_ERROR_TEXT_NO_NEWLINE(dbgParams); \
		PrintUnicode((uniText)); \
	} \
	while(0)

// removes the brackets from a macro tuple 
#define INT_REMOVE_BRACKETS_0
#define INT_REMOVE_BRACKETS_1(string, param1) string, param1
#define INT_REMOVE_BRACKETS_2(string, param1, param2) string, param1, param2
#define INT_REMOVE_BRACKETS_3(string, param1, param2, param3) string, param1, param2, param3

#define ERROR_TEXT_U(Count, params, uniText) \
	INT_ERROR_TEXT_U \
	( \
		CONCAT(INT_REMOVE_BRACKETS_, Count) params, \
		uniText \
	)

#ifdef DBG

#define DBG_TEXT(...) ERROR_TEXT(__VA_ARGS__)
#define DBG_TEXT_U(Count, params, uniText) ERROR_TEXT_U(Count, params, uniText)

#else

#define DBG_TEXT(string, ...) ((void)0)
#define DBG_TEXT_U(Count, params, uniText) ((void)0)

#endif

#endif
