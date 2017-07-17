#ifndef WINLIB_SIMPLE_ASSERT_H
#define WINLIB_SIMPLE_ASSERT_H

#pragma once

// support macros
#define CONCAT_INT(a, b) a##b
#define CONCAT(a, b) CONCAT_INT(a, b)

#define WSTRINGIFY_INT(a) CONCAT(L, #a)
#define WSTRINGIFY(a) WSTRINGIFY_INT(a)
#define STRINGIFY_INT(a) #a
#define STRINGIFY(a) STRINGIFY_INT(a)

// compile time assert
#define CASSERT(expr) \
	static const char CONCAT(cassertTester, __COUNTER__) [(expr) ? 1 : -1]

#if defined(DEBUG) || defined(_DEBUG) || defined(_DBG)

#ifdef _MSC_VER
__if_not_exists(OutputDebugStringA)
{
	// so we don't have to include the windows header files
	extern "C" __declspec(dllimport) void __stdcall OutputDebugStringA(const char*);
}
#endif

#define ASSERT(expr) \
	do \
	{ \
		if(!(expr)) \
		{ \
			OutputDebugStringA( \
			"Assertion Failed: " \
			#expr \
			"\nFile: " \
			__FILE__ \
			", line " \
			STRINGIFY(__LINE__) \
			", function \"" \
			__FUNCTION__ \
			"\".\n" \
			); \
			DebugBreak(); \
		} \
	} while(0) \
	/*  */

#else

#define ASSERT(expr)

#endif

#endif
