#ifndef HOOKS_H
#define HOOKS_H

#pragma once

extern "C"
{
	NTSTATUS GetWindowsHookInfo(PIRP pIrp, ULONG_PTR* pulpBytesWritten);
}

#ifdef ALLOC_PRAGMA
#pragma alloc_text(PAGE, GetWindowsHookInfo)
#endif

#endif
