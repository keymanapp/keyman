
#define STRICT
#include <windows.h>
#include "imlib.h"

HMODULE hModuleKeyman = 0;
KMSetOutputProc KMSetOutput = NULL;
KMGetContextProc KMGetContext = NULL;
KMQueueActionProc KMQueueAction = NULL;
KMDisplayIMProc KMDisplayIM = NULL;
KMHideIMProc KMHideIM = NULL;
KMGetActiveKeyboardProc KMGetActiveKeyboard = NULL;
KMGetKeyboardPathProc KMGetKeyboardPath = NULL;

BOOL WINAPI IntKMGetKeyboardPath(PSTR kbdname, PSTR buf, DWORD len)
{
	wsprintf(buf, "c:\\keyman\\imsample\\%s.kmx", kbdname);
	return TRUE;
}

BOOL PrepIM(void)
{
	if(hModuleKeyman != 0) return TRUE;

#ifdef _WIN64
  hModuleKeyman = GetModuleHandle("keyman64.dll");
#else
  hModuleKeyman = GetModuleHandle("keyman32.dll");
#endif

	if(hModuleKeyman == 0)
	{
		KMGetKeyboardPath = IntKMGetKeyboardPath;
		return TRUE;
	}

	KMSetOutput = (KMSetOutputProc) GetProcAddress(hModuleKeyman, "KMSetOutput");
	if(!KMSetOutput) return FALSE;

	KMGetContext = (KMGetContextProc) GetProcAddress(hModuleKeyman, "KMGetContext");
	if(!KMGetContext) return FALSE;

	KMQueueAction = (KMQueueActionProc) GetProcAddress(hModuleKeyman, "KMQueueAction");
	if(!KMQueueAction) return FALSE;

	KMDisplayIM = (KMDisplayIMProc) GetProcAddress(hModuleKeyman, "KMDisplayIM");
	if(!KMDisplayIM) return FALSE;

	KMHideIM = (KMHideIMProc) GetProcAddress(hModuleKeyman, "KMHideIM");
	if(!KMHideIM) return FALSE;

	KMGetActiveKeyboard = (KMGetActiveKeyboardProc) GetProcAddress(hModuleKeyman, "KMGetActiveKeyboard");
	if(!KMGetActiveKeyboard) return FALSE;

	KMGetKeyboardPath = (KMGetKeyboardPathProc) GetProcAddress(hModuleKeyman, "KMGetKeyboardPath");
	if(!KMGetKeyboardPath) return FALSE;

	return TRUE;
}

// IMDefWindowProc returns TRUE if the window procedure should just return lResult, and
// not process further.

HCURSOR GetHitTestCursor(HWND hwnd, LRESULT ht, LPRECT sizerect)
{
	LPSTR cr = NULL;

	memset(sizerect, 0, sizeof(RECT));

	switch(ht)
	{
		case HTTOP:			cr = IDC_SIZENS;   sizerect->top = 1; break;
		case HTTOPLEFT:		cr = IDC_SIZENWSE; sizerect->top = sizerect->left = 1; break;
		case HTTOPRIGHT:	cr = IDC_SIZENESW; sizerect->top = sizerect->right = 1; break;
		case HTBOTTOM:		cr = IDC_SIZENS;   sizerect->bottom = 1; break;
		case HTBOTTOMLEFT:  cr = IDC_SIZENESW; sizerect->bottom = sizerect->left = 1; break;
		case HTBOTTOMRIGHT: cr = IDC_SIZENWSE; sizerect->bottom = sizerect->right = 1; break;
		case HTLEFT:		cr = IDC_SIZEWE;   sizerect->left = 1; break;
		case HTRIGHT:		cr = IDC_SIZEWE;   sizerect->right = 1; break;
		default: return (HCURSOR) GetClassLongPtr(hwnd, GCLP_HCURSOR);
	}

	return LoadCursor(NULL, cr);
}


BOOL IMDefWindowProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam, LRESULT *lResult)
{
	static POINT FLastPoint;
	static BOOL FMoveWindow=FALSE, FSizeWindow=FALSE;
	static RECT sizerect;
	static HCURSOR FHitTestCursor;

	RECT rect;
	POINTS pt;
	POINT pt2;
	LRESULT ht;

	switch(msg)
	{
	case WM_CREATE:
		PostMessage(hwnd, WM_NCACTIVATE, TRUE, 0);
		return FALSE;

	case WM_NCACTIVATE:
		*lResult = DefWindowProc(hwnd, msg, TRUE, lParam);
		return TRUE;

	case WM_NCHITTEST:
		*lResult = HTCLIENT;

		if(FMoveWindow || FSizeWindow) return TRUE;

		ht = DefWindowProc(hwnd, msg, wParam, lParam);
		SetCursor(FHitTestCursor = GetHitTestCursor(hwnd, ht, &sizerect));
		return TRUE;

	case WM_MOUSEACTIVATE:
		*lResult = MA_NOACTIVATE;
		return TRUE;

	case WM_LBUTTONDOWN:
		pt = MAKEPOINTS(lParam); pt2.x = pt.x; pt2.y = pt.y;
		ClientToScreen(hwnd, &pt2);

		FMoveWindow = FALSE;
		FSizeWindow = FALSE;

		ht = DefWindowProc(hwnd, WM_NCHITTEST, 0, MAKELONG(pt2.x, pt2.y));
		switch(ht)
		{
		case HTCAPTION:
			FMoveWindow = TRUE;
			SetCapture(hwnd);
			FLastPoint = pt2;
			SetCursor(FHitTestCursor);
			return TRUE;

		case HTTOP:
		case HTTOPLEFT:
		case HTTOPRIGHT:
		case HTBOTTOM:
		case HTBOTTOMLEFT:
		case HTBOTTOMRIGHT:
		case HTLEFT:
		case HTRIGHT:
			FSizeWindow = TRUE;
			SetCapture(hwnd);
			FLastPoint = pt2;
			SetCursor(FHitTestCursor);
			return TRUE;

		default:
			return ht != HTCLIENT;
		}

	case WM_MOUSEMOVE:
		GetWindowRect(hwnd, &rect);
		pt = MAKEPOINTS(lParam); pt2.x = pt.x; pt2.y = pt.y;
		ClientToScreen(hwnd, &pt2);

		if(FMoveWindow)
		{
			MoveWindow(hwnd,
				rect.left+pt2.x - FLastPoint.x,
				rect.top+pt2.y - FLastPoint.y,
				rect.right-rect.left,
				rect.bottom-rect.top,
				TRUE);
			FLastPoint = pt2;
			SetCursor(FHitTestCursor);
			return TRUE;
		}
		else if(FSizeWindow)
		{
			POINT pt3 = pt2; pt3.x -= FLastPoint.x; pt3.y -= FLastPoint.y;
			RECT r2;
			r2.left = rect.left + pt3.x*sizerect.left;
			r2.top = rect.top + pt3.y*sizerect.top;
			r2.right = rect.right + pt3.x*sizerect.right;
			r2.bottom = rect.bottom + pt3.y*sizerect.bottom;
			if(r2.right - r2.left < 16)
			{
				r2.right = rect.right; r2.left = rect.left;
			}
			else FLastPoint.x = pt2.x;
			if(r2.bottom - r2.top < 10)
			{
				r2.bottom = rect.bottom; r2.top = rect.top;
			}
			else FLastPoint.y = pt2.y;

			MoveWindow(hwnd,
				r2.left,
				r2.top,
				r2.right-r2.left,
				r2.bottom-r2.top,
				TRUE);

			SetCursor(FHitTestCursor);
			return TRUE;
		}

		return DefWindowProc(hwnd, WM_NCHITTEST, 0, MAKELONG(pt2.x, pt2.y)) != HTCLIENT;

	case WM_LBUTTONUP:
		if(FMoveWindow || FSizeWindow)
		{
			FMoveWindow = FSizeWindow = FALSE;
			ReleaseCapture();
			return TRUE;
		}

		pt = MAKEPOINTS(lParam); pt2.x = pt.x; pt2.y = pt.y;
		ClientToScreen(hwnd, &pt2);

		return DefWindowProc(hwnd, WM_NCHITTEST, 0, MAKELONG(pt2.x, pt2.y)) != HTCLIENT;
	}

	return FALSE;
}
