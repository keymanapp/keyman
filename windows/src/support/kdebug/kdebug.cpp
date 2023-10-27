#define STRICT

#include <windows.h>
#include <stdio.h>

HWND hwnd;
HINSTANCE hinst;
LPSTR prm;

LRESULT CALLBACK MainWndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam);

BOOL Keyman_Initialise(HWND hwnd, BOOL FSingleApp);
BOOL Keyman_Exit();
BOOL Keyman_ForceKeyboard(PSTR s);
BOOL Keyman_StopForcingKeyboard();

/************************************************************************************************************
 * DLL entry functions
 ************************************************************************************************************/

int PASCAL WinMain(HINSTANCE hinstance, HINSTANCE hprevinst, LPSTR cmdline, int nCmdShow)
{
	WNDCLASSW wc;

	hinst = hinstance;
	prm = cmdline;

	wc.style = CS_VREDRAW | CS_HREDRAW | CS_DBLCLKS;
	wc.lpfnWndProc = (WNDPROC) MainWndProc;
	wc.cbClsExtra = 0;
	wc.cbWndExtra = 0;
	wc.hInstance = hinst;
	wc.hIcon = NULL;
	wc.hCursor = LoadCursor(NULL, IDC_ARROW);
	wc.hbrBackground = (HBRUSH) GetStockObject(WHITE_BRUSH);
	wc.lpszMenuName = NULL;
	wc.lpszClassName = L"KeymanDebug";
	if(!RegisterClassW(&wc)) return FALSE;

	hwnd = CreateWindowW(L"KeymanDebug", L"Keyman Debugger",
		WS_OVERLAPPEDWINDOW | WS_VISIBLE,
		CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, 0, NULL, hinst, NULL);
	if(!hwnd) return FALSE;

	MSG msg;

	while(GetMessageW(&msg, NULL, NULL, NULL))
	{
		TranslateMessage(&msg);
		DispatchMessageW(&msg);
	}

	return 0;
}

#define  MAXBUF	1024
WCHAR buf[MAXBUF];
HFONT hFont;

void CreateFont()
{
	HDC hDC = GetDC(GetDesktopWindow());
	SetMapMode(hDC, MM_TEXT);
	int lfpixelsy = GetDeviceCaps(hDC, LOGPIXELSY);
	ReleaseDC(GetDesktopWindow(), hDC);

	LOGFONT font;

	font.lfHeight         = -MulDiv(36, lfpixelsy, 72);
	font.lfWidth          = 0;
	font.lfEscapement     = 0;
	font.lfOrientation    = 0;
	font.lfWeight         = FW_NORMAL;
	font.lfItalic         = FALSE;
	font.lfUnderline      = FALSE;
	font.lfStrikeOut      = FALSE;
	font.lfCharSet        = DEFAULT_CHARSET;
	font.lfOutPrecision   = OUT_DEFAULT_PRECIS;
	font.lfClipPrecision  = CLIP_DEFAULT_PRECIS;
	font.lfQuality        = DEFAULT_QUALITY;
	font.lfPitchAndFamily = DEFAULT_PITCH | FF_DONTCARE;
	strcpy(font.lfFaceName, "SIL Yi");

	hFont = CreateFontIndirect(&font);
}


LRESULT CALLBACK MainWndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	switch(msg)
	{
	case WM_DESTROY:
		Keyman_StopForcingKeyboard();
		Keyman_Exit();
		DeleteObject(hFont);

		PostQuitMessage(0);
		return 0;

	case WM_CREATE:
		if(!Keyman_Initialise(hwnd, TRUE)) return -1;
		return 0;

	case WM_SHOWWINDOW:
		PostMessage(hwnd, WM_USER+100, 0, 0);
		break;

	case WM_USER+100:

		if(!Keyman_ForceKeyboard(prm)) DestroyWindow(hwnd);
		CreateFont();
		return 0;

	case WM_KEYDOWN:
		if(wParam == VK_ESCAPE)
			*buf = 0;
		else break;
		InvalidateRect(hwnd, NULL, TRUE);
		return 0;
	case WM_CHAR:
		if(*buf != 0 && wParam == 8)
		{
			WCHAR *p = wcschr(buf, 0) - 1;
			*p = 0;
			InvalidateRect(hwnd, NULL, TRUE);
		}
		else if(wcslen(buf) < MAXBUF-1)
		{
			WCHAR *p = wcschr(buf, 0);
			*p++ = wParam;
			*p = 0;
			InvalidateRect(hwnd, NULL, TRUE);
		}
		else MessageBeep(0);
		return 0;
	case WM_PAINT:
		{
			RECT rect;
			PAINTSTRUCT ps;
			HFONT hOldFont;
			HDC hDC = BeginPaint(hwnd, &ps);
			hOldFont = (HFONT) SelectObject(hDC, hFont);
			GetClientRect(hwnd, &rect);
			rect.left++; rect.top++; rect.right--; rect.bottom--;
			DrawTextW(hDC, buf, wcslen(buf), &rect, DT_WORDBREAK);
			SelectObject(hDC, hOldFont);
			EndPaint(hwnd, &ps);
			return 0;
		}

	}

	return DefWindowProc(hwnd, msg, wParam, lParam);
}


typedef BOOL (WINAPI *Keyman_ForceKeyboardProc)(PSTR s);
typedef BOOL (WINAPI *Keyman_StopForcingKeyboardProc)();
typedef BOOL (WINAPI *Keyman_InitialiseProc)(HWND hwnd, BOOL FSingleApp);
typedef BOOL (WINAPI *Keyman_ExitProc)();

BOOL FLoad = FALSE, FInitKeyman = FALSE;
char FKeyman32Path[260] = "";

BOOL Keyman_Initialise(HWND hwnd, BOOL FSingleApp)
{
	HINSTANCE hkeyman;
	Keyman_InitialiseProc ki;

	FLoad = FALSE;

	hkeyman = GetModuleHandle("keyman32.dll");
	if(!hkeyman)
	{
		hkeyman = LoadLibrary("c:\\keyman\\5.0\\bin\\dist\\keyman32.dll");
		if(!hkeyman) return FALSE;
		FLoad = TRUE;
	}

	ki = (Keyman_InitialiseProc) GetProcAddress(hkeyman, "Keyman_Initialise");
	if(!ki) return FALSE;
	if(!(*ki)(hwnd, FSingleApp))
	{
		if(FLoad) FreeLibrary(hkeyman);
		return FALSE;
	}

	FInitKeyman = TRUE;
	return TRUE;
}

BOOL Keyman_Exit()
{
	HINSTANCE hkeyman;
	Keyman_ExitProc ke;

	if(!FInitKeyman) return TRUE;
	hkeyman = GetModuleHandle("keyman32.dll");
	if(!hkeyman) return FALSE;

	ke = (Keyman_ExitProc) GetProcAddress(hkeyman, "Keyman_Exit");
	if(!ke) return FALSE;
	if(!(*ke)()) return FALSE;
	if(FLoad) FreeLibrary(hkeyman);
	FInitKeyman = FALSE;
	FLoad = FALSE;
	return TRUE;
}

BOOL Keyman_ForceKeyboard(PSTR s)
{
	HINSTANCE hkeyman;
	Keyman_ForceKeyboardProc fk;

	hkeyman = GetModuleHandle("keyman32.dll");
	if(!hkeyman) return FALSE;

	fk = (Keyman_ForceKeyboardProc) GetProcAddress(hkeyman, "Keyman_ForceKeyboard");
	if(!fk) return FALSE;

	return (*fk)(s);
}

BOOL Keyman_StopForcingKeyboard()
{
	HINSTANCE hkeyman;
	Keyman_StopForcingKeyboardProc sfk;

	hkeyman = GetModuleHandle("keyman32.dll");
	if(!hkeyman) return FALSE;

	sfk = (Keyman_StopForcingKeyboardProc) GetProcAddress(hkeyman, "Keyman_StopForcingKeyboard");
	if(!sfk) return FALSE;
	return (*sfk)();
}
