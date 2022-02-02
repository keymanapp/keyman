/*
  Name:             imsample
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:
  Description:
  Create Date:      24 Oct 2012

  Modified Date:    24 Oct 2012
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          24 Oct 2012 - mcdurdin - I3481 - V9.0 - Eliminate unsafe calls in C++
*/

#define STRICT

#include <windows.h>
#include <stdio.h>
#include "imlib.h"

HWND hwnd;
HWND hwndChild;
HINSTANCE hinst;
UINT wm_keymanim_close = 0, wm_keymanim_contextchanged;

BOOL FIMWindowUse = FALSE;
BOOL FIMWindowAlwaysVisible = FALSE;
BOOL FIMWindowActive = FALSE;

struct rule
{
	int contextlen;
	WCHAR *context;
	WCHAR key;
	int outputlen;
	BOOL drawvalid;
	WCHAR *outputs[10]; // max ten outputs
};

struct group
{
	char name[32];
	int nrules;
	rule *rules;
};

struct keyboard
{
	char name[128];

	LOGFONT font;
	HFONT hFont;

	int maxoutputs;
	int ngroups;
	int gridx, gridy;
	group *groups;
};

rule *currule = NULL;
keyboard *keyboards = NULL, *curkbd = NULL;
int nkeyboards;

//BOOL FWindow, FWindowAlways, FInWindow = FALSE;

LRESULT CALLBACK IMSampleChildWndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam);
LRESULT CALLBACK IMSampleWndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam);
extern "C" BOOL __declspec(dllexport) WINAPI KeymanIMReloadConfig(PSTR keyboardname);
void WriteRegSetting(PSTR text, int value);
int ReadRegSetting(PSTR text);
void UnloadRules(PSTR KeyboardName);
BOOL LoadRules(PSTR KeyboardName);
void DeleteKeyboard(PSTR keyboardname);
void CreateKeyboard(PSTR keyboardname);


/************************************************************************************************************
 * DLL entry functions
 ************************************************************************************************************/

BOOL WINAPI DllEntryPoint(HINSTANCE hinstDLL, DWORD fdwReason, LPVOID lpvReserved)
{
	return TRUE;
}

extern "C" BOOL __declspec(dllexport) WINAPI KeymanIMDestroy(PSTR keyboardname)
{
	UnregisterClass("KM_IMSample", hinst);
	DeleteKeyboard(keyboardname);
	DestroyWindow(hwnd);
	UnloadRules(keyboardname);
	currule = NULL;
	curkbd = NULL;
	FIMWindowActive = FALSE;
	return TRUE;
}

extern "C" BOOL __declspec(dllexport) WINAPI KeymanIMInit(PSTR keyboardname)
{
	WNDCLASS wc;

	wm_keymanim_close = RegisterWindowMessage("WM_KEYMANIM_CLOSE");
	wm_keymanim_contextchanged = RegisterWindowMessage("WM_KEYMANIM_CONTEXTCHANGED");

	hinst = GetModuleHandle(NULL);

	if(!GetClassInfo(hinst, "KM_IMSample", &wc))
	{
		wc.style = CS_VREDRAW | CS_HREDRAW | CS_DBLCLKS;// | CS_SAVEBITS;
		wc.lpfnWndProc = (WNDPROC) IMSampleWndProc;
		wc.cbClsExtra = 0;
		wc.cbWndExtra = 0;
		wc.hInstance = hinst;
		wc.hIcon = NULL;
		wc.hCursor = LoadCursor(NULL, IDC_ARROW);
		wc.hbrBackground = (HBRUSH) GetStockObject(WHITE_BRUSH);
		wc.lpszMenuName = NULL;
		wc.lpszClassName = "KM_IMSample";
		if(!RegisterClass(&wc)) return FALSE;

		wc.lpfnWndProc = (WNDPROC) IMSampleChildWndProc;
		wc.lpszClassName = "KM_IMSampleChild";
		if(!RegisterClass(&wc)) return FALSE;
	}

	POINT pt;
	pt.x = ReadRegSetting("imsample x");
	pt.y = ReadRegSetting("imsample y");

	hwnd = CreateWindowEx(WS_EX_TOOLWINDOW | WS_EX_TOPMOST, "KM_IMSample", "Keyman Sample IM",
		WS_POPUP | WS_BORDER | WS_CAPTION | WS_SYSMENU | WS_THICKFRAME,
		pt.x, pt.y, 200, 120, 0, NULL, hinst, NULL);

	if(!hwnd) return FALSE;

	return KeymanIMReloadConfig(keyboardname);
}

extern "C" BOOL __declspec(dllexport) WINAPI KeymanIMReloadConfig(PSTR keyboardname)
{
	currule = NULL;
	curkbd = NULL;
	FIMWindowActive = FALSE;

	FIMWindowUse = ReadRegSetting("ShowIMWindow");
	FIMWindowAlwaysVisible = ReadRegSetting("ShowIMWindowAlways");

	if(!PrepIM()) return FALSE;

	CreateKeyboard(keyboardname);
	LoadRules(keyboardname);

	if(!FIMWindowUse || !FIMWindowAlwaysVisible) (*KMHideIM)();

	return TRUE;
}

/************************************************************************************************************
 * IM Events
 ************************************************************************************************************/

extern "C" BOOL __declspec(dllexport) WINAPI KeymanIMActivate(PSTR keyboardname)
{
	currule = NULL;
	curkbd = NULL;
	FIMWindowActive = FALSE;
	if(!PrepIM()) return FALSE;
	if(FIMWindowUse && FIMWindowAlwaysVisible) (*KMDisplayIM)(hwnd, TRUE);

	int kbi;

	char kbdname[260];

	if(!(*KMGetActiveKeyboard)(kbdname, 256)) return FALSE;

	for(kbi = 0; kbi < nkeyboards; kbi++)
		if(!_stricmp(keyboards[kbi].name, kbdname)) break;
	if(kbi == nkeyboards) return FALSE;
	curkbd = &keyboards[kbi];

	return TRUE;
}

extern "C" BOOL __declspec(dllexport) WINAPI KeymanIMDeactivate(PSTR keyboardname)
{
	currule = NULL;
	curkbd = NULL;
	FIMWindowActive = FALSE;
	if(!PrepIM()) return FALSE;
	(*KMHideIM)();
	return TRUE;
}

/************************************************************************************************************
 * Window management functions
 ************************************************************************************************************/

void FinishEdit()
{
	FIMWindowActive = FALSE;
	currule = NULL;
	PostMessage(hwndChild, WM_USER+200, 0, 0);
	PostMessage(hwnd, wm_keymanim_close, 1, !FIMWindowAlwaysVisible);
	if(FIMWindowAlwaysVisible) (*KMDisplayIM)(hwnd, TRUE);
}


LRESULT CALLBACK IMSampleWndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	RECT rect;
	POINTS pt;
	LRESULT lResult;
	PAINTSTRUCT ps;
	HDC hDC;

	// Handle standard IM functions -- such as window activation, etc.
	if(IMDefWindowProc(hwnd, msg, wParam, lParam, &lResult)) return lResult;

	if(msg == wm_keymanim_contextchanged)
		PostMessage(hwndChild, WM_USER+200, 0, 0);

	switch(msg)
	{
	case WM_CREATE:
		pt.x = ReadRegSetting("imsample x");
		pt.y = ReadRegSetting("imsample y");

		if(pt.x > 0 && pt.y > 0)
			SetWindowPos(hwnd, 0, pt.x, pt.y, 0, 0, SWP_NOSIZE | SWP_NOACTIVATE | SWP_NOZORDER);

		GetClientRect(hwnd, &rect);
		hwndChild = CreateWindowEx(WS_EX_CLIENTEDGE,
			"KM_IMSampleChild", "", WS_BORDER | WS_CHILD | /*WS_HSCROLL |*/ WS_VSCROLL | WS_VISIBLE,
			2, 2, rect.right - 2, rect.bottom - 2, hwnd, 0, hinst, NULL);
		PostMessage(hwndChild, WM_USER+200, 0, 0);

		return 0;

	case WM_SHOWWINDOW:
		if(wParam) PostMessage(hwndChild, WM_USER+200, 0, 0);
		break;

	case WM_DESTROY:
		GetWindowRect(hwnd, &rect);
		WriteRegSetting("imsample x", rect.left);
		WriteRegSetting("imsample y", rect.top);

		return 0;

	case WM_KEYDOWN:
		if(!FIMWindowActive || !currule) return 0;
		if(wParam == VK_ESCAPE || wParam == VK_BACK)
			FinishEdit();
		return 0;

	case WM_CHAR:
		if(!FIMWindowActive || !currule) return 0;

		if(wParam >= '1' && wParam <= '9')
		{
			wParam -= '1';
			if(!currule->outputs[wParam]) return 0;
			(*KMSetOutput)(currule->outputs[wParam], currule->contextlen);
			FinishEdit();
		}
		return 0;

	case WM_SIZE:
		MoveWindow(hwndChild, 2, 2, LOWORD(lParam) - 2, HIWORD(lParam) - 2, TRUE);
		return 0;

	case WM_PAINT:
		hDC = BeginPaint(hwnd, &ps);
		GetClientRect(hwnd, &rect);
		FillRect(hDC, &rect, GetSysColorBrush(COLOR_BTNFACE));
		EndPaint(hwnd, &ps);

		return 0;
	}

	return DefWindowProc(hwnd, msg, wParam, lParam);
}

struct
{
	int cellcount, visiblecolcount, visiblerowcount, xi, yi;
} wz;

void CalcScrollSize()
{
	RECT rect;

	if(!curkbd) { wz.cellcount = 0; return; }
  if (!curkbd->groups) {
    wz.cellcount = 0;
    return;
  }
	GetClientRect(hwndChild, &rect);

	if(FIMWindowActive && currule)
		for(wz.cellcount = 0; wz.cellcount < 10; wz.cellcount++) { if(!currule->outputs[wz.cellcount]) break; }
	else
	{
		WCHAR buf[48];
		if(!(*KMGetContext)(buf, 48)) buf[0] = 0;
		int buflen = (int)wcslen(buf);

		wz.cellcount = 0;
                int i;

		for(i = 0; i < curkbd->groups[0].nrules; i++)
		{
			rule *r = &curkbd->groups[0].rules[i];
			r->drawvalid = r->contextlen == 0 ||
				(buflen >= r->contextlen && !wcscmp(buf+(buflen-r->contextlen), r->context));
			if(r->drawvalid) wz.cellcount++;
		}
	}

	wz.visiblecolcount = (curkbd->gridx != 0 ? rect.right / curkbd->gridx : rect.right);
  wz.visiblerowcount = (curkbd->gridy != 0 ? rect.bottom / curkbd->gridy : rect.bottom);

	wz.xi = (wz.visiblecolcount !=0 ) ? (wz.cellcount-1) % wz.visiblecolcount : 2;
	wz.yi = (wz.visiblecolcount !=0 ) ? (wz.cellcount-1) / wz.visiblecolcount : 2;

	SCROLLINFO si;
	si.cbSize = sizeof(SCROLLINFO);
	si.fMask = SIF_ALL | SIF_DISABLENOSCROLL;
	si.nMin = 0;
	si.nMax = max(0, wz.yi - wz.visiblerowcount + 1);
	si.nPage = wz.visiblerowcount-1;
	si.nPos = 0;
	SetScrollInfo(hwndChild, SB_VERT, &si, FALSE);
}


LRESULT CALLBACK IMSampleChildWndProc(HWND hwndChild, UINT msg, WPARAM wParam, LPARAM lParam)
{
	static int cx, cy;
	RECT rect;
	POINTS pt;
	PAINTSTRUCT ps;
	HDC hDC;
	int n, nPos;
	LRESULT lResult;

	switch(msg)
	{
	case WM_NCHITTEST:
		lResult = DefWindowProc(hwndChild, msg, wParam, lParam);
		if(lResult != HTCLIENT && lResult != HTVSCROLL && lResult != HTHSCROLL) return HTNOWHERE;

		return lResult;

	case WM_MOUSEACTIVATE:
		return MA_NOACTIVATE;

	case WM_SIZE:
	case WM_USER+200:
		CalcScrollSize();
		InvalidateRect(hwndChild, NULL, TRUE);

		return 0;

	case WM_LBUTTONUP:
		if(!PrepIM()) return DefWindowProc(hwndChild, msg, wParam, lParam);
		if(!curkbd) return 0;
    if (!curkbd->groups) {
      return 0;
    }
		GetClientRect(hwndChild, &rect);

		pt = MAKEPOINTS(lParam);

		pt.x /= cx;
		pt.y /= cy;

		n = pt.y*(rect.right/cx) + pt.x;

		if(!FIMWindowActive || !currule)
		{       int i, j;
			for(i = 0, j = 0; i < curkbd->groups[0].nrules; i++)
				if(curkbd->groups[0].rules[i].drawvalid)
				{
					if(j == n) break;
					j++;
				}

			if(i == curkbd->groups[0].nrules) return 0;

			currule = &curkbd->groups[0].rules[i];
			FIMWindowActive = TRUE;
			PostMessage(hwndChild, WM_USER+200, 0, 0);
			(*KMDisplayIM)(hwnd, FALSE);
		}
		else
		{
			if(n > 9) return 0;
			if(!currule->outputs[n]) return 0;
			(*KMSetOutput)(currule->outputs[n], currule->contextlen);

			FinishEdit();
		}
		return 0;

	case WM_VSCROLL:
		nPos = HIWORD(wParam);
		if(nPos == 0 && LOWORD(wParam) != SB_THUMBTRACK && LOWORD(wParam) != SB_THUMBPOSITION)
			nPos = GetScrollPos(hwndChild, SB_VERT);
		switch(LOWORD(wParam))
		{
			case SB_BOTTOM:			nPos = wz.yi - wz.visiblerowcount + 1; break;
			case SB_ENDSCROLL:		break;
			case SB_LINEDOWN:		nPos++; break;
			case SB_LINEUP:			nPos--; break;
			case SB_PAGEDOWN:		nPos += wz.visiblerowcount - 1; break;
			case SB_PAGEUP:			nPos -= wz.visiblerowcount - 1; break;
			case SB_THUMBPOSITION:	break;
			case SB_THUMBTRACK:		break;
			case SB_TOP:				nPos = 0; break;
		}
		if(nPos < 0) nPos = 0;
		if(nPos > wz.yi - wz.visiblerowcount + 1) nPos = wz.yi - wz.visiblerowcount + 1;

		SetScrollPos(hwndChild, SB_VERT, nPos, TRUE);
		InvalidateRect(hwndChild, NULL, TRUE);
		UpdateWindow(hwndChild);

		return 0;
	case WM_PAINT:
    if (!curkbd || !curkbd->hFont || !curkbd->groups || !PrepIM()) {
      return DefWindowProc(hwndChild, msg, wParam, lParam);
    }

		int vpos = GetScrollPos(hwndChild, SB_VERT) * curkbd->gridy;

		hDC = BeginPaint(hwndChild, &ps);
		GetClientRect(hwndChild, &rect);
		if(curkbd)
		{
			HFONT hOldFont = (HFONT) SelectObject(hDC, curkbd->hFont);

			cx = curkbd->gridx;
			cy = curkbd->gridy;
                        int i;
			for(i = 0; i < wz.visiblecolcount; i++)
			{
				MoveToEx(hDC, (i+1)*cx, -vpos + 0, NULL);
				if(i <= wz.xi || wz.yi > 0) LineTo(hDC, (i+1)*cx, -vpos + (i <= wz.xi ? cy * (wz.yi+1) : cy * wz.yi)+1);
			}
			for(i = 0; i <= wz.yi; i++)
			{
				MoveToEx(hDC, 0, -vpos + (i+1)*cy, NULL);
				LineTo(hDC, (i < wz.yi ? cx * wz.visiblecolcount : cx * (wz.xi+1))+1, -vpos + (i+1)*cy);
			}

			if(FIMWindowActive && currule)
			{
                                int i, x, y;
				for(i = 0, x = 0, y = 0; i < 10; i++)
				{
					if(!currule->outputs[i]) break;
					char buf[2]; buf[0] = i+'1'; buf[1] = 0;
					SelectObject(hDC, GetStockObject(SYSTEM_FONT));
					TextOut(hDC, x * cx + 1, -vpos + y * cy+1, buf, 1);
					SelectObject(hDC, curkbd->hFont);

					SIZE sz2;

					GetTextExtentPoint32W(hDC, currule->outputs[i], (int)wcslen(currule->outputs[i]), &sz2);

					TextOutW(hDC, x * cx + (cx-sz2.cx)/2, -vpos + (y+1) * cy - sz2.cy - 2,
						currule->outputs[i], (int)wcslen(currule->outputs[i]));

					if(++x >= wz.visiblecolcount) { x = 0; y++; }
				}
			}
			else
			{
				SelectObject(hDC, curkbd->hFont);
                                int i,x,y;
				for(i = 0, x = 0, y = 0; i < curkbd->groups[0].nrules; i++)
				{
					rule *r = &curkbd->groups[0].rules[i];
					if(!r->drawvalid) continue;

					SIZE sz2;

					GetTextExtentPoint32W(hDC, &r->key, 1, &sz2);

					TextOutW(hDC, x * cx + (cx-sz2.cx)/2, -vpos + (y+1) * cy - sz2.cy - 2, &r->key, 1);

					if(++x >= wz.visiblecolcount) { x = 0; y++; }
				}
			}
			SelectObject(hDC, hOldFont);
		}
		else
			Rectangle(hDC, -1, -1, rect.right + 1, rect.bottom + 1);

		/*char str[128];
		wsprintf(str, "%d %d %d %d", wz.xi, wz.yi, wz.visiblecolcount, wz.visiblerowcount);
		TextOut(hDC, 0, 0, str, strlen(str));*/

		EndPaint(hwndChild, &ps);
		return 0;
	}

	return DefWindowProc(hwndChild, msg, wParam, lParam);
}



BOOL FindRule(group *g, rule **rp, WCHAR KeyChar)
{
	if (!g || !rp) {
    return FALSE;
  }
  WCHAR buf[48];
	if(!(*KMGetContext)(buf, 48)) return FALSE;
	rule *r = g->rules;
	int buflen = (int)wcslen(buf);
  int i;
	for(i = 0; i < g->nrules; i++)
	{
		if(r[i].key == KeyChar && buflen >= r[i].contextlen && !wcscmp(buf+(buflen-r[i].contextlen), r[i].context))
		{
			*rp = &r[i];
			return TRUE;
		}
	}
	return FALSE;
}

BOOL PostKey(WCHAR KeyChar, WORD KeyStroke)
{
	if(KeyChar != 0)
	{
		WCHAR buf[2];
		buf[0] = KeyChar;
		buf[1] = 0;
		(*KMSetOutput)(buf, 0);
	}
	else
	{
		if(KeyStroke == VK_BACK)
			(*KMQueueAction)(QIT_BACK, 0);
		else
			(*KMQueueAction)(QIT_VKEYDOWN, KeyStroke);
	}
	return TRUE;
}

extern "C" BOOL __declspec(dllexport) WINAPI DFWindow(HWND hwndFocus, WORD KeyStroke, WCHAR KeyChar, DWORD shiftFlags)
{
	if(!curkbd) return FALSE;
  if (!curkbd->groups) {
    return FALSE;
  }

	if(!FindRule(&curkbd->groups[0], &currule, KeyChar))
	{
		if(FIMWindowAlwaysVisible)
			PostMessage(hwndChild, WM_USER+200, 0, 0);
		//UpdateWindow(hwndChild);
		return PostKey(KeyChar, KeyStroke);
	}

	FIMWindowActive = TRUE;
	PostMessage(hwndChild, WM_USER+200, 0, 0);
	(*KMDisplayIM)(hwnd, FALSE);
	return TRUE;
}

extern "C" BOOL __declspec(dllexport) WINAPI DFText(HWND hwndFocus, WORD KeyStroke, WCHAR KeyChar, DWORD shiftFlags)
{
	WCHAR buf[32];
	if (!curkbd || !curkbd->groups) {
    return FALSE;
  }
	if(!currule)
	{
		if(!FindRule(&curkbd->groups[0], &currule, KeyChar))
		{
			PostKey(KeyChar, KeyStroke);
			return TRUE;
		}
		else
		{
			(*KMSetOutput)(L"[", 0);
			for(int j = 0; currule->outputs[j]; j++)
			{
				if(j>0) (*KMSetOutput)(L" ", 0);
				buf[0] = j+'1'; buf[1] = 0;
				(*KMSetOutput)(buf, 0);
				(*KMSetOutput)(currule->outputs[j], 0);
			}
			(*KMSetOutput)(L"]", 0);
		}
	}
	else if(KeyChar >= '1' && KeyChar <= '9')
	{
		KeyChar -= '1';
		if(currule->outputs[KeyChar])
		{
			(*KMSetOutput)(currule->outputs[KeyChar], currule->outputlen);
			currule = NULL;
		}
	}
	else if(KeyStroke == VK_BACK || KeyStroke == VK_ESCAPE)
	{
		(*KMSetOutput)(L"", currule->outputlen);
		currule = NULL;
	}
	return TRUE;
}

extern "C" BOOL __declspec(dllexport) WINAPI DF(HWND hwndFocus, WORD KeyStroke, WCHAR KeyChar, DWORD shiftFlags)
{
	if(!PrepIM()) return FALSE;

	if(FIMWindowUse)
		return DFWindow(hwndFocus, KeyStroke, KeyChar, shiftFlags);
	else
		return DFText(hwndFocus, KeyStroke, KeyChar, shiftFlags);
}


/************************************************************************************************************
 * IM Configuration
 ************************************************************************************************************/

extern "C" BOOL __declspec(dllexport) WINAPI KeymanIMConfigure(PSTR keyboardname, HWND hwndParent)
{
	int n = MessageBox(hwndParent, "Do you want to have a menu window?",
		"IMSample", MB_YESNOCANCEL);

	switch(n)
	{
	case IDYES:
		n = MessageBox(hwndParent, "Do you want to have the menu window always open?",
			"IMSample", MB_YESNOCANCEL);

		switch(n)
		{
		case IDYES:
			WriteRegSetting("ShowIMWindowAlways", 1);
			break;

		case IDNO:
			WriteRegSetting("ShowIMWindowAlways", 0);
			break;

		default:
			return TRUE;
		}

		WriteRegSetting("ShowIMWindow", 1);
		break;

	case IDNO:
		WriteRegSetting("ShowIMWindow", 0);
		break;
	}

	return TRUE;
}

void WriteRegSetting(PSTR text, int value)
{
	HKEY hkey;
  if (RegOpenKeyEx(HKEY_CURRENT_USER, "Software\\Keyman\\Keyman Engine\\Active Keyboards\\imsample",
		0, KEY_ALL_ACCESS, &hkey) == ERROR_SUCCESS)
	{
		RegSetValueEx(hkey, text, 0, REG_DWORD, (PBYTE) &value, 4);
		RegCloseKey(hkey);
	}
}

int ReadRegSetting(PSTR text)
{
	HKEY hkey;
	if(RegOpenKeyEx(HKEY_CURRENT_USER, "Software\\Keyman\\Keyman Engine\\Active Keyboards\\imsample",
		0, KEY_ALL_ACCESS, &hkey) == ERROR_SUCCESS)
	{
		unsigned long value, sz = 4, tp = REG_DWORD;
		RegQueryValueEx(hkey, text, 0, &tp, (PBYTE) &value, &sz);
		RegCloseKey(hkey);
		return value;
	}
	return 0;
}



char *strtokquoted(char *str, char *token, char *quotes)
{ //slow-n-ugly but quick to write ;-)
	static char *buf = NULL;
	if(!str)
		if(!buf) return NULL;
		else str = buf;

	char *q = str;
	if(!*q) return q;

	while(strchr(token, *q)) q++;

	if(strchr(quotes, *q))	//quoted substring
	{
		char *r = strchr(q+1, *q);
		if(!r) return NULL; // unterminated quotes
		r++;
		if(!strchr(token, *r)) return NULL; // close of quoted string does not culminate in token
		if(*r != 0)
		{
			*r++ = 0;
			while(*r != 0 && strchr(token, *r)) r++;
		}
		if(*r == 0) buf = NULL; else buf = r;
		return q;
	}
	else
	{
		char *r = q;
		while(!strchr(token, *r) && *r != 0) r++;
		if(*r != 0)
		{
			*r++ = 0;
			while(*r != 0 && strchr(token, *r)) r++;
		}
		if(*r == 0) buf = NULL; else buf = r;
		return q;
	}
}


PWSTR newempty()
{
	PWSTR buf = new WCHAR[1];
	buf[0] = 0;
	return buf;
}

PWSTR extstr(char *p)
{
	if(*p == '"' || *p == '\'')
	{
		p++;
		char *q = strchr(p, *(p-1));
		if(!q) return newempty();
		if(q == p) return newempty();
		*q = 0;
		int n = MultiByteToWideChar(CP_ACP, 0, p, (int)strlen(p), NULL, 0);
		PWCHAR buf = new WCHAR[n+1];
		MultiByteToWideChar(CP_ACP, 0, p, (int)strlen(p), buf, n);
		buf[n] = 0;
		return buf;
	}
	else
	{
		WCHAR buf[64];
		int i = 0, n;
		while(*p)
		{
			switch(*p)
			{
			case ' ':
				p++; break;
			case 'U':
			case 'u':
				p++;
				if(*p != '+') return newempty();

			case 'x':
			case 'X':
				p++;

				n = strtol(p, &p, 16);
				if(!p) return newempty();
				buf[i++] = n;
				break;

			case 'd':
			case 'D':
				p++;

				n = strtol(p, &p, 10);
				if(!p) return newempty();
				buf[i++] = n;
				break;

			default:
				return newempty();
			}
		}
		buf[i] = 0;
		PWSTR p = new WCHAR[i+1];
		wcscpy_s(p, i+1, buf);  // I3481
		return p;
	}
}

void UnloadRules(PSTR KeyboardName)
{
	int nkbi;
	for(nkbi = 0; nkbi < nkeyboards; nkbi++)
		if(!_stricmp(KeyboardName, keyboards[nkbi].name)) break;

	if(nkbi == nkeyboards) return;

	keyboard *kbi = &keyboards[nkbi];

	if(kbi->hFont) DeleteObject(kbi->hFont);
	kbi->hFont = NULL;

  if (kbi->groups == NULL) {
          return;
  }

  int i;
	for(i = 0; i < kbi->ngroups; i++)
	{
		for(int j = 0; j < kbi->groups[i].nrules; j++)
		{
			delete kbi->groups[i].rules[j].context;
			for(int k = 0; k < 10; k++) if(kbi->groups[i].rules[j].outputs[k]) delete kbi->groups[i].rules[j].outputs[k];
		}
		if(kbi->groups[i].nrules > 0) delete kbi->groups[i].rules;
	}
	if(kbi->ngroups > 0) delete kbi->groups;
	kbi->ngroups = 0;
	kbi->groups = NULL;
}

BOOL LoadRules(PSTR KeyboardName)
{
	BOOL FGroups = FALSE, FRules = FALSE, FConfig = FALSE;
	char str[512];
	int nkbi, lfpixelsy;

	UnloadRules(KeyboardName); // Free existing rule base

	for(nkbi = 0; nkbi < nkeyboards; nkbi++)
		if(!_stricmp(KeyboardName, keyboards[nkbi].name)) break;

	if(nkbi == nkeyboards) return FALSE;

	keyboard *kbi = &keyboards[nkbi];

	kbi->maxoutputs = 0;
	kbi->ngroups = 0;

	HDC hDC = GetDC(GetDesktopWindow());
	SetMapMode(hDC, MM_TEXT);
	lfpixelsy = GetDeviceCaps(hDC, LOGPIXELSY);
	ReleaseDC(GetDesktopWindow(), hDC);

	kbi->font.lfHeight         = -MulDiv(10, lfpixelsy, 72);
	kbi->font.lfWidth          = 0;
	kbi->font.lfEscapement     = 0;
	kbi->font.lfOrientation    = 0;
	kbi->font.lfWeight         = FW_NORMAL;
	kbi->font.lfItalic         = FALSE;
	kbi->font.lfUnderline      = FALSE;
	kbi->font.lfStrikeOut      = FALSE;
	kbi->font.lfCharSet        = DEFAULT_CHARSET;
	kbi->font.lfOutPrecision   = OUT_DEFAULT_PRECIS;
	kbi->font.lfClipPrecision  = CLIP_DEFAULT_PRECIS;
	kbi->font.lfQuality        = DEFAULT_QUALITY;
	kbi->font.lfPitchAndFamily = DEFAULT_PITCH | FF_DONTCARE;
  strcpy_s(kbi->font.lfFaceName, _countof(kbi->font.lfFaceName), "Arial");  // I3481

	char drive[_MAX_DRIVE], dir[_MAX_DIR], buf[260];

	if(!(*KMGetKeyboardPath)(KeyboardName, buf, 260)) return FALSE;
	_splitpath_s(buf, drive, _MAX_DRIVE, dir, _MAX_DIR, NULL, 0, NULL, 0);  // I3481
	_makepath_s(buf, _countof(buf), drive, dir, "imsample", ".txt");  // I3481

  FILE *fp = NULL;
	if(fopen_s(&fp, buf, "rt") != 0) return FALSE;  // I3481
	if(!fp) return FALSE;

	while(fgets(str, 512, fp))
	{
		if(strchr(str, '\n')) *strchr(str, '\n') = 0;

		if(str[0] == '#' || str[0] == 0) continue;

		if(str[0] == '[')
		{
			FConfig = FALSE; FGroups = FALSE; FRules = FALSE;
			if(!_stricmp(str, "[config]"))      FConfig = TRUE;
			else if(!_stricmp(str, "[groups]")) FGroups = TRUE;
			else if(!_stricmp(str, "[rules]"))  FRules = TRUE;
		}
		else if(FConfig)
		{
      char *tokcontext = NULL;
			char *p = strtok_s(str, "= ", &tokcontext); if(!p) continue;  // I3481
			char *q = strtok_s(NULL, "\n", &tokcontext); if(!q) continue;  // I3481
			if(!_stricmp(p, "font"))
			{
				strncpy_s(kbi->font.lfFaceName, _countof(kbi->font.lfFaceName), q, LF_FACESIZE-1);
				kbi->font.lfFaceName[LF_FACESIZE-1] = 0;
			}
			else if(!_stricmp(p, "size"))
			{
				int n = atoi(q); if(n == 0) n = 10;
				kbi->font.lfHeight = -MulDiv(n, lfpixelsy, 72);
			}
			else if(!_stricmp(p, "gridx"))
				kbi->gridx = atoi(q);
			else if(!_stricmp(p, "gridy"))
				kbi->gridy = atoi(q);
		}
		else if(FGroups)
		{
			group *g2;
			g2 = new group[kbi->ngroups+1];
			if(kbi->ngroups > 0)
			{
				memcpy(g2, kbi->groups, sizeof(group)*kbi->ngroups);
				delete kbi->groups;
			}
			kbi->groups = g2;

			kbi->groups[kbi->ngroups].rules = NULL;
			kbi->groups[kbi->ngroups].nrules = 0;
			strncpy_s(kbi->groups[kbi->ngroups].name, _countof(kbi->groups[kbi->ngroups].name), str, 31);  // I3481
			kbi->groups[kbi->ngroups++].name[31] = 0;
		}
		else if(FRules)
		{
			int i;
			PSTR pgroup = strtokquoted(str, ", \t",    "\"\'"); if(!pgroup) continue; // silently fail on that line [for now]
			PSTR pcontext = strtokquoted(NULL, ", \t", "\"\'"); if(!pcontext) continue;
			PSTR pkey = strtokquoted(NULL, ", \t",     "\"\'"); if(!pkey) continue;
			PSTR poutput = strtokquoted(NULL, ", \t",  "\"\'"); if(!poutput) continue; // must be at least one output

			for(i = 0; i < kbi->ngroups; i++)
				if(!_stricmp(kbi->groups[i].name, pgroup)) break;

			if(i == kbi->ngroups) continue;

			if((kbi->groups[i].nrules % 10) == 0)
			{
				rule *r2 = new rule[kbi->groups[i].nrules+10];
				if(kbi->groups[i].nrules > 0)
				{
					memcpy(r2, kbi->groups[i].rules, sizeof(rule)*kbi->groups[i].nrules);
					delete kbi->groups[i].rules;
				}
				kbi->groups[i].rules = r2;
			}

			rule *r2 = &kbi->groups[i].rules[kbi->groups[i].nrules++];

			r2->context = extstr(pcontext);
			r2->contextlen = (int)wcslen(r2->context);
			PWSTR pp = extstr(pkey);
			r2->key = pp[0];
			delete pp;

			i = 0; r2->outputlen = 1;
			while(poutput && i < 10)
			{
				r2->outputs[i++] = extstr(poutput);
				r2->outputlen += (int)wcslen(r2->outputs[i-1]) + 2;
				poutput = strtokquoted(NULL, ", ", "\"\'");
			}
			if(i > kbi->maxoutputs) kbi->maxoutputs = i;
			for(; i < 10; i++) r2->outputs[i] = NULL;
		}
		// fail silently on unrecognised lines
	}

	fclose(fp);
	kbi->hFont = CreateFontIndirect(&kbi->font);

	return TRUE;
}

void DeleteKeyboard(PSTR keyboardname)
{
	int nkbi;

	for(nkbi = 0; nkbi < nkeyboards; nkbi++)
		if(!_stricmp(keyboardname, keyboards[nkbi].name)) break;

	if(nkbi == nkeyboards) return;

	if(nkbi < nkeyboards)
		memmove(&keyboards[nkbi], &keyboards[nkbi+1], sizeof(keyboard)*(nkeyboards-nkbi-1));
	nkeyboards--;
	if(nkeyboards == 0)
	{
		delete keyboards;
		keyboards = NULL;
	}
}

void CreateKeyboard(PSTR keyboardname)
{
	int nkbi;

	for(nkbi = 0; nkbi < nkeyboards; nkbi++)
		if(!_stricmp(keyboardname, keyboards[nkbi].name)) break;

	if(nkbi == nkeyboards)
	{
		keyboard *kb2 = new keyboard[nkeyboards + 1];
		if(nkeyboards > 0)
		{
			memcpy(kb2, keyboards, sizeof(keyboard) * nkeyboards);
			delete keyboards;
		}
		keyboards = kb2;
		strncpy_s(keyboards[nkeyboards].name, _countof(keyboards[nkeyboards].name), keyboardname, 127);  // I3481
		keyboards[nkeyboards].name[127] = 0;
    keyboards[nkeyboards].groups    = NULL;
    keyboards[nkeyboards].hFont     = NULL;
		nkeyboards++;
	}
}

