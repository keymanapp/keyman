#ifndef WINDOW_H
#define WINDOW_H

#pragma once

bool IsWin32kUnchanged();
void ListWindowMessages(HWND hwnd);
void ListActiveWindows();
DWORD GetThreadProcessFileName(DWORD threadId, std::wstring& processName);

#endif
