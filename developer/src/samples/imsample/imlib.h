
#ifndef _IMLIB_H
#define _IMLIB_H

#define QIT_VKEYDOWN	0
#define QIT_VKEYUP		1
//#define QIT_VSHIFTDOWN	2 // deprecated see #11925
//#define QIT_VSHIFTUP	3
#define QIT_CHAR		4
#define QIT_DEADKEY		5
#define QIT_BELL		6
#define QIT_BACK		7

typedef BOOL (WINAPI *KMSetOutputProc)(PWSTR buf, DWORD backlen);
typedef BOOL (WINAPI *KMGetContextProc)(PWSTR buf, DWORD len);
typedef BOOL (WINAPI *KMQueueActionProc)(int action, DWORD dwData);
typedef BOOL (WINAPI *KMDisplayIMProc)(HWND hwnd, BOOL FStayOpen);
typedef BOOL (WINAPI *KMGetActiveKeyboardProc)(PSTR kbdname, DWORD len);
typedef BOOL (WINAPI *KMHideIMProc)();
typedef BOOL (WINAPI *KMGetKeyboardPathProc)(PSTR kbdname, PSTR buf, DWORD len);

extern HMODULE hModuleKeyman;
extern KMSetOutputProc KMSetOutput;
extern KMGetContextProc KMGetContext;
extern KMQueueActionProc KMQueueAction;
extern KMDisplayIMProc KMDisplayIM;
extern KMHideIMProc KMHideIM;
extern KMGetActiveKeyboardProc KMGetActiveKeyboard;
extern KMGetKeyboardPathProc KMGetKeyboardPath;

BOOL PrepIM(void);
BOOL IMDefWindowProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam, LRESULT *lResult);

#endif
