/*
  Name:             savekeyboard
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      18 Sep 2007

  Modified Date:    8 Apr 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          18 Sep 2007 - mcdurdin - Fix group and store offset bugs
                    18 Sep 2007 - mcdurdin - Fix bug with nomatch written as match
                    12 Oct 2007 - mcdurdin - Add Keyman 7.0 system store names
                    06 Nov 2007 - mcdurdin - Use the filename of the bitmap as stored in the .kmx
                    19 Sep 2008 - mcdurdin - Add WINDOWSLANGUAGES 7.1 system store
                    27 Aug 2012 - mcdurdin - I3438 - V9.0 - Add support for custom virtual keys Created
                    12 Oct 2012 - mcdurdin - I3467 - V9.0 - Upgrade KMDECOMP to compile with KM9 source tree
                    08 Apr 2015 - mcdurdin - I4652 - V9.0 - Decompiler crashes if an unrecognised system store is encountered
*/


#include "pch.h"

#include <stdio.h>

#include "../../../common/windows/cpp/include/legacy_kmx_file.h"
#include "../../../common/windows/cpp/include/legacy_kmx_memory.h"
#include "../../../common/windows/cpp/include/vkeys.h"

#define SSN__PREFIX		L"&"

const char *UTF16Sig = "\xFF\xFE";

extern void Err(char *p);
void SaveBitmapFile(LPBYTE lpBitmap, DWORD cbBitmap, char *bmpfile);

LPKEYBOARD g_kbd;

PWCHAR wcscat2(PWCHAR c1, size_t sz, const PWCHAR c2)
{
	wcscat_s(c1, sz, c2);
	return wcschr(c1, 0);
}



const wchar_t*  VKeyNames[] = {
// Key Codes
	L"K_?00",				// &H0
	L"K_LBUTTON",			// &H1
	L"K_RBUTTON",			// &H2
	L"K_CANCEL",		   	// &H3
	L"K_MBUTTON",			// &H4
	L"K_?05",				// &H5
	L"K_?06",				// &H6
	L"K_?07",				// &H7
	L"K_BKSP",	    		// &H8
	L"K_TAB",	    		// &H9
	L"K_?0A",				// &HA
	L"K_?0B",				// &HB
	L"K_KP5",		    	// &HC
	L"K_ENTER",				// &HD
	L"K_?0E",				// &HE
	L"K_?0F",				// &HF
	L"K_SHIFT",				// &H10
	L"K_CONTROL",			// &H11
	L"K_ALT",				// &H12
	L"K_PAUSE",				// &H13
	L"K_CAPS",				// &H14
	L"K_KANJI?15",			// &H15
	L"K_KANJI?16",			// &H16
	L"K_KANJI?17",			// &H17
	L"K_KANJI?18",			// &H18
	L"K_KANJI?19",			// &H19
	L"K_?1A",				// &H1A
	L"K_ESC",				// &H1B
	L"K_KANJI?1C",			// &H1C
	L"K_KANJI?1D",			// &H1D
	L"K_KANJI?1E",			// &H1E
	L"K_KANJI?1F",			// &H1F
	L"K_SPACE",				// &H20
	L"K_PGUP",				// &H21
	L"K_PGDN",				// &H22
	L"K_END",				// &H23
	L"K_HOME",				// &H24
	L"K_LEFT",				// &H25
	L"K_UP",				// &H26
	L"K_RIGHT",				// &H27
	L"K_DOWN",				// &H28
	L"K_SEL",				// &H29
	L"K_PRINT",				// &H2A
	L"K_EXEC",				// &H2B
	L"K_PRTSCN",			// &H2C
	L"K_INS",				// &H2D
	L"K_DEL",				// &H2E
	L"K_HELP",				// &H2F
	L"K_0",					// &H30
	L"K_1",					// &H31
	L"K_2",					// &H32
	L"K_3",					// &H33
	L"K_4",					// &H34
	L"K_5",					// &H35
	L"K_6",					// &H36
	L"K_7",					// &H37
	L"K_8",					// &H38
	L"K_9",					// &H39
	L"K_?3A",				// &H3A
	L"K_?3B",				// &H3B
	L"K_?3C",				// &H3C
	L"K_?3D",				// &H3D
	L"K_?3E",				// &H3E
	L"K_?3F",				// &H3F
	L"K_?40",				// &H40

	L"K_A",					// &H41
	L"K_B",					// &H42
	L"K_C",					// &H43
	L"K_D",					// &H44
	L"K_E",					// &H45
	L"K_F",					// &H46
	L"K_G",					// &H47
	L"K_H",					// &H48
	L"K_I",					// &H49
	L"K_J",					// &H4A
	L"K_K",					// &H4B
	L"K_L",					// &H4C
	L"K_M",					// &H4D
	L"K_N",					// &H4E
	L"K_O",					// &H4F
	L"K_P",					// &H50
	L"K_Q",					// &H51
	L"K_R",					// &H52
	L"K_S",					// &H53
	L"K_T",					// &H54
	L"K_U",					// &H55
	L"K_V",					// &H56
	L"K_W",					// &H57
	L"K_X",					// &H58
	L"K_Y",					// &H59
	L"K_Z",					// &H5A
	L"K_?5B",				// &H5B
	L"K_?5C",				// &H5C
	L"K_?5D",				// &H5D
	L"K_?5E",				// &H5E
	L"K_?5F",				// &H5F
	L"K_NP0",				// &H60
	L"K_NP1",				// &H61
	L"K_NP2",				// &H62
	L"K_NP3",				// &H63
	L"K_NP4",				// &H64
	L"K_NP5",				// &H65
	L"K_NP6",				// &H66
	L"K_NP7",				// &H67
	L"K_NP8",				// &H68
	L"K_NP9",				// &H69
	L"K_NPSTAR",			// &H6A
	L"K_NPPLUS",			// &H6B
	L"K_SEPARATOR",			// &H6C
	L"K_NPMINUS",			// &H6D
	L"K_NPDOT",				// &H6E
	L"K_NPSLASH",			// &H6F
	L"K_F1",				// &H70
	L"K_F2",				// &H71
	L"K_F3",				// &H72
	L"K_F4",				// &H73
	L"K_F5",				// &H74
	L"K_F6",				// &H75
	L"K_F7",				// &H76
	L"K_F8",				// &H77
	L"K_F9",				// &H78
	L"K_F10",				// &H79
	L"K_F11",				// &H7A
	L"K_F12",				// &H7B
	L"K_F13",				// &H7C
	L"K_F14",				// &H7D
	L"K_F15",				// &H7E
	L"K_F16",				// &H7F
	L"K_F17",				// &H80
	L"K_F18",				// &H81
	L"K_F19",				// &H82
	L"K_F20",				// &H83
	L"K_F21",				// &H84
	L"K_F22",				// &H85
	L"K_F23",				// &H86
	L"K_F24",				// &H87

	L"K_?88",				// &H88
	L"K_?89",				// &H89
	L"K_?8A",				// &H8A
	L"K_?8B",				// &H8B
	L"K_?8C",				// &H8C
	L"K_?8D",				// &H8D
	L"K_?8E",				// &H8E
	L"K_?8F",				// &H8F

	L"K_NUMLOCK",			// &H90
	L"K_SCROLL",			// &H91

	L"K_?92",				// &H92
	L"K_?93",				// &H93
	L"K_?94",				// &H94
	L"K_?95",				// &H95
	L"K_?96",				// &H96
	L"K_?97",				// &H97
	L"K_?98",				// &H98
	L"K_?99",				// &H99
	L"K_?9A",				// &H9A
	L"K_?9B",				// &H9B
	L"K_?9C",				// &H9C
	L"K_?9D",				// &H9D
	L"K_?9E",				// &H9E
	L"K_?9F",				// &H9F
	L"K_?A0",				// &HA0
	L"K_?A1",				// &HA1
	L"K_?A2",				// &HA2
	L"K_?A3",				// &HA3
	L"K_?A4",				// &HA4
	L"K_?A5",				// &HA5
	L"K_?A6",				// &HA6
	L"K_?A7",				// &HA7
	L"K_?A8",				// &HA8
	L"K_?A9",				// &HA9
	L"K_?AA",				// &HAA
	L"K_?AB",				// &HAB
	L"K_?AC",				// &HAC
	L"K_?AD",				// &HAD
	L"K_?AE",				// &HAE
	L"K_?AF",				// &HAF
	L"K_?B0",				// &HB0
	L"K_?B1",				// &HB1
	L"K_?B2",				// &HB2
	L"K_?B3",				// &HB3
	L"K_?B4",				// &HB4
	L"K_?B5",				// &HB5
	L"K_?B6",				// &HB6
	L"K_?B7",				// &HB7
	L"K_?B8",				// &HB8
	L"K_?B9",				// &HB9

	L"K_COLON",				// &HBA
	L"K_EQUAL",				// &HBB
	L"K_COMMA",				// &HBC
	L"K_HYPHEN",			// &HBD
	L"K_PERIOD",			// &HBE
	L"K_SLASH",				// &HBF
	L"K_BKQUOTE",			// &HC0

	L"K_?C1",				// &HC1
	L"K_?C2",				// &HC2
	L"K_?C3",				// &HC3
	L"K_?C4",				// &HC4
	L"K_?C5",				// &HC5
	L"K_?C6",				// &HC6
	L"K_?C7",				// &HC7
	L"K_?C8",				// &HC8
	L"K_?C9",				// &HC9
	L"K_?CA",				// &HCA
	L"K_?CB",				// &HCB
	L"K_?CC",				// &HCC
	L"K_?CD",				// &HCD
	L"K_?CE",				// &HCE
	L"K_?CF",				// &HCF
	L"K_?D0",				// &HD0
	L"K_?D1",				// &HD1
	L"K_?D2",				// &HD2
	L"K_?D3",				// &HD3
	L"K_?D4",				// &HD4
	L"K_?D5",				// &HD5
	L"K_?D6",				// &HD6
	L"K_?D7",				// &HD7
	L"K_?D8",				// &HD8
	L"K_?D9",				// &HD9
	L"K_?DA",				// &HDA

	L"K_LBRKT",				// &HDB
	L"K_BKSLASH",			// &HDC
	L"K_RBRKT",				// &HDD
	L"K_QUOTE",				// &HDE
	L"K_oDF",				// &HDF
	L"K_oE0",				// &HE0
	L"K_oE1",				// &HE1
	L"K_oE2",				// &HE2
	L"K_oE3",				// &HE3
	L"K_oE4",				// &HE4

	L"K_?E5",				// &HE5

	L"K_oE6",				// &HE6

	L"K_?E7",				// &HE7
	L"K_?E8",				// &HE8

	L"K_oE9",				// &HE9
	L"K_oEA",				// &HEA
	L"K_oEB",				// &HEB
	L"K_oEC",				// &HEC
	L"K_oED",				// &HED
	L"K_oEE",				// &HEE
	L"K_oEF",				// &HEF
	L"K_oF0",				// &HF0
	L"K_oF1",				// &HF1
	L"K_oF2",				// &HF2
	L"K_oF3",				// &HF3
	L"K_oF4",				// &HF4
	L"K_oF5",				// &HF5

	L"K_?F6",				// &HF6
	L"K_?F7",				// &HF7
	L"K_?F8",				// &HF8
	L"K_?F9",				// &HF9
	L"K_?FA",				// &HFA
	L"K_?FB",				// &HFB
	L"K_?FC",				// &HFC
	L"K_?FD",				// &HFD
	L"K_?FE",				// &HFE
	L"K_?FF"				// &HFF
	};



// TODO: consolidate with list in compiler.cpp
const PWCHAR StoreTokens[] = {   // I4652
	L"",
	SSN__PREFIX L"BITMAP",
	SSN__PREFIX L"COPYRIGHT",
	SSN__PREFIX L"HOTKEY",
	SSN__PREFIX L"LANGUAGE",
	SSN__PREFIX L"LAYOUT",
	SSN__PREFIX L"MESSAGE",
	SSN__PREFIX L"NAME",
	SSN__PREFIX L"VERSION",
	SSN__PREFIX L"CAPSONONLY",
	SSN__PREFIX L"CAPSALWAYSOFF",
	SSN__PREFIX L"SHIFTFREESCAPS",
	SSN__PREFIX L"LANGUAGENAME",
	L"",
	L"",
	SSN__PREFIX L"ETHNOLOGUECODE",
	L"",
	SSN__PREFIX L"MNEMONICLAYOUT",
	SSN__PREFIX L"INCLUDECODES",
	SSN__PREFIX L"OLDCHARPOSMATCHING",
	L"",
	L"",
	L"",
	L"",
	SSN__PREFIX L"VISUALKEYBOARD",
	SSN__PREFIX L"KMW_RTL",
	SSN__PREFIX L"KMW_HELPFILE",
	SSN__PREFIX L"KMW_HELPTEXT",
	SSN__PREFIX L"KMW_EMBEDJS",
  SSN__PREFIX L"WINDOWSLANGUAGES",
  L"",
  SSN__PREFIX L"PLATFORM",    // read only  // I3430
  SSN__PREFIX L"BASELAYOUT",  // read only  // I3430
  SSN__PREFIX L"LAYER",       // read-write via set?  // I3430
  L"",                        // I3438
  SSN__PREFIX L"LAYOUTFILE",  // I3483
  SSN__PREFIX L"KEYBOARDVERSION",   // I4140
	SSN__PREFIX L"KMW_EMBEDCSS",
  SSN__PREFIX L"TARGETS",   // I4504
  SSN__PREFIX L"CASEDKEYS",
  L"", // TSS_BEGIN_NEWCONTEXT
  L"", // TSS_BEGIN_POSTKEYSTROKE
  SSN__PREFIX L"NEWLAYER",
  SSN__PREFIX L"OLDLAYER",
	NULL
};

static_assert(_countof(StoreTokens) == TSS__MAX + 2);

PWCHAR storename(int num)
{
	static WCHAR buf[256];
	LPSTORE sp = &g_kbd->dpStoreArray[num-1];
	if(sp->dwSystemID != 0)
	{
		if(StoreTokens[sp->dwSystemID][0])
			wsprintfW(buf, L"%s", StoreTokens[sp->dwSystemID]);
		else
			wsprintfW(buf, L"&%d", sp->dwSystemID);
	}
	else if(sp->dpName != NULL)
		wsprintfW(buf, L"%s", sp->dpName);
	else
		wsprintfW(buf, L"store%d", num);
	return buf;
}

PWCHAR storevalue(int num)
{
	LPSTORE sp = &g_kbd->dpStoreArray[num-1];
  return sp->dpString;
}

PWCHAR groupname(int num)
{
	static WCHAR buf[256];
	if((DWORD)num > g_kbd->cxGroupArray) return L"???";

	LPGROUP gp = &g_kbd->dpGroupArray[num-1];
	if(gp->dpName != NULL)
		wsprintfW(buf, L"%s", gp->dpName);
	else
		wsprintfW(buf, L"group%d", num);
	return buf;
}

PWCHAR flagstr(int flag)
{
	static WCHAR buf[256];
	*buf = 0;
	if(flag & LCTRLFLAG)       wcscat2(buf, _countof(buf), L"LCTRL ");
	if(flag & RCTRLFLAG)       wcscat2(buf, _countof(buf), L"RCTRL ");
	if(flag & LALTFLAG)        wcscat2(buf, _countof(buf), L"LALT ");
	if(flag & RALTFLAG)        wcscat2(buf, _countof(buf), L"RALT ");
	if(flag & K_SHIFTFLAG)     wcscat2(buf, _countof(buf), L"SHIFT ");
	if(flag & K_CTRLFLAG)      wcscat2(buf, _countof(buf), L"CTRL ");
	if(flag & K_ALTFLAG)       wcscat2(buf, _countof(buf), L"ALT ");
	if(flag & CAPITALFLAG)     wcscat2(buf, _countof(buf), L"CAPS ");
	if(flag & NOTCAPITALFLAG)  wcscat2(buf, _countof(buf), L"NCAPS ");
	if(flag & NUMLOCKFLAG)     wcscat2(buf, _countof(buf), L"NUM ");
	if(flag & NOTNUMLOCKFLAG)  wcscat2(buf, _countof(buf), L"NNUM ");
	if(flag & SCROLLFLAG)      wcscat2(buf, _countof(buf), L"SCROLL ");
	if(flag & NOTSCROLLFLAG)   wcscat2(buf, _countof(buf), L"NSCROLL ");
	return buf;
}

PCWCHAR GetVKeyName(LPKEY key)  // I3438
{
  static WCHAR buf[100];
  if(key->Key <= VK__MAX)
    return VKeyNames[key->Key];

  wsprintfW(buf, L"%d", key->Key - 256);  //TODO: Support getting the key name from the VK Dictionary
  return buf;
}

PWCHAR KeyString(LPKEY key)
{
	static WCHAR buf[100];

	if(key->ShiftFlags & ISVIRTUALKEY)
	{
		if(key->ShiftFlags & VIRTUALCHARKEY)
			wsprintfW(buf, L"[%s%c%c%c] ", flagstr(key->ShiftFlags), key->Key == L'"' ? L'\'' : L'"',
				key->Key, key->Key == L'"' ? L'\'' : L'"');
		else
			wsprintfW(buf, L"[%s%s] ", flagstr(key->ShiftFlags), GetVKeyName(key));////; //, VKeyNames[key->Key]);  // I3438
	}
	else
	{
		if(key->Key == L'"') wsprintfW(buf, L"'\"' ");
		else wsprintfW(buf, L"\"%c\" ", key->Key);
	}

	return buf;
}

PWCHAR ifvalue(WCHAR ch)
{
  if(ch == 1) return L"!=";
  return L"=";
}

#define BUFSIZE 2048
PWCHAR ExtString(PWCHAR str)
{
	static WCHAR buf[2][BUFSIZE], bufpointer = 0;	// allows for multiple strings in one printf
												// dodgy hack?

	PWCHAR p;
	int inquotes = 0;

	bufpointer = !bufpointer;
	p = buf[bufpointer]; *p = 0;
  PWCHAR q = p;

	for(; *str; str++)
	{
		if(*str == UC_SENTINEL)
		{
			str++;
			if(inquotes) p = wcscat2(q, BUFSIZE, L"\" ");
			inquotes = 0;
			switch(*str)
			{
			case CODE_ANY:
				str++;
				wsprintfW(p, L"any(%s) ", storename(*str));
				p = wcschr(p, 0);
				break;
			case CODE_INDEX:
				str++;
				wsprintfW(p, L"index(%s, %d) ", storename(*str), *(str+1));
				str++;
				p = wcschr(p, 0);
				break;
			case CODE_CONTEXT:
				p = wcscat2(q, BUFSIZE, L"context ");
				break;
			case CODE_NUL:
				p = wcscat2(q, BUFSIZE, L"nul ");
				break;
			case CODE_USE:
				str++;
				wsprintfW(p, L"use(%s) ", groupname(*str));
				p = wcschr(p, 0);
				break;
			case CODE_RETURN:
				p = wcscat2(q, BUFSIZE, L"return ");
				break;
			case CODE_BEEP:
				p = wcscat2(q, BUFSIZE, L"beep ");
				break;
			case CODE_DEADKEY:
				str++;
				wsprintfW(p, L"deadkey(%d) ", *str);
				p = wcschr(p, 0);
				break;
			case CODE_EXTENDED:
				str++;
				if(*str & VIRTUALCHARKEY)
					wsprintfW(p, L"[%s%c%c%c] ", flagstr(*str), *(str+1) == L'"' ? L'\'' : L'"',
						*(str+1), *(str+1) == L'"' ? L'\'' : L'"');
				else
        {
          if(*(str+1) > VK__MAX)  // I3438
            //TODO: Get extended key value
            wsprintfW(p, L"[%s %d] ", flagstr(*str), *(str+1)-VK__MAX-1);
          else
					  wsprintfW(p, L"[%s%s] ", flagstr(*str), VKeyNames[*(str+1)]);
        }
				str+=2; // skip UC_SENTINEL_EXTENDEDEND
				p = wcschr(p, 0);
				break;
			case CODE_SWITCH:
				p = wcscat2(q, BUFSIZE, L"switch<deprecated> ");
				break;
			case CODE_KEY:
				p = wcscat2(q, BUFSIZE, L"key<deprecated> ");
				break;
			case CODE_CLEARCONTEXT:
				p = wcscat2(q, BUFSIZE, L"clearcontext ");
				break;
			case CODE_CALL:
				str++;
				wsprintfW(p, L"call(%s) ", storename(*str));
				p = wcschr(p, 0);
				break;
			case CODE_CONTEXTEX:
				str++;
				wsprintfW(p, L"context(%d) ", *str);
				p = wcschr(p, 0);
				break;
      case CODE_NOTANY:
				str++;
				wsprintfW(p, L"notany(%s) ", storename(*str));
				p = wcschr(p, 0);
				break;

      case CODE_SETOPT:
				str++;
				wsprintfW(p, L"set(%s = '%s') ", storename(*str), storevalue(*(str+1)));
				p = wcschr(p, 0);
        str++;
				break;

      case CODE_IFOPT:
				str++;
				wsprintfW(p, L"if(%s%s '%s') ", storename(*str), ifvalue(*(str+1)), storevalue(*(str+2)));
				p = wcschr(p, 0);
        str+=2;
        break;
      case CODE_SAVEOPT:
				str++;
				wsprintfW(p, L"save(%s) ", storename(*str));
				p = wcschr(p, 0);
        break;

      case CODE_RESETOPT:
				str++;
				wsprintfW(p, L"reset(%s) ", storename(*str));
				p = wcschr(p, 0);
        break;

      case CODE_IFSYSTEMSTORE:
        str++;
        wsprintfW(p, L"if(%s %s '%s')", StoreTokens[*str-1], ifvalue(*(str+1)), storevalue(*(str+2)));
        p = wcschr(p, 0);
        str+=2;
        break;

      case CODE_SETSYSTEMSTORE:
        str++;
        wsprintfW(p, L"set(%s = '%s')", StoreTokens[*str-1], storevalue(*(str+1)));
        p = wcschr(p, 0);
        str++;
        break;

			default:
				p = wcscat2(q, BUFSIZE, L"unknown() ");
				break;
			}
		}
		else
		{
			if(*str == L'"')
			{
				if(inquotes) p = wcscat2(q, BUFSIZE, L"\" ");
				inquotes = 0;
				p = wcscat2(q, BUFSIZE, L"'\"' ");
			}
			else if(*str < 32)
			{
				if(inquotes) p = wcscat2(q, BUFSIZE, L"\" ");
				inquotes = 0;
				wsprintfW(p, L"x%x ", *str);
				p = wcschr(p, 0);
			}
			else
			{
				if(!inquotes)
				{
					inquotes = 1;
					p = wcscat2(q, BUFSIZE, L"\"");
				}
				*p++ = *str;
				*p = 0;
			}
		}
	}

	if(inquotes) p = wcscat2(q, BUFSIZE, L"\" ");
	return buf[bufpointer];
}

void wr(FILE *fp, PWSTR buf)
{
	fwrite(buf, wcslen(buf) * 2, 1, fp);
}

int SaveKeyboardSource(LPKEYBOARD kbd, LPBYTE lpBitmap, DWORD cbBitmap, char* filename, char* bmpfile)
{
  PWCHAR buf;
  FILE* fp;
  LPSTORE sp;
  LPGROUP gp;
  LPKEY kp;
  unsigned int i, j;
  char bmpbuf[_MAX_PATH];
  char bmp_drive[_MAX_DRIVE], bmp_dir[_MAX_DIR], bmp_filename[_MAX_FNAME], bmp_ext[_MAX_EXT];

	buf = new WCHAR[2048];

	g_kbd = kbd;

  if (fopen_s(&fp, filename, "wb") != 0) {
    Err("Unable to create output file.");
    return 3;
  }

  fwrite(UTF16Sig, 2, 1, fp);

	wsprintfW(buf, L"c Keyboard created by KMDECOMP\n"); wr(fp, buf);
	wsprintfW(buf, L"c\n"); wr(fp, buf);
	wsprintfW(buf, L"c Meta details: Registered=%d; Version=%x\n", kbd->IsRegistered, kbd->version); wr(fp, buf);
	wsprintfW(buf, L"c               Flags=%x; HotKey=%x\n", kbd->dwFlags, kbd->dwHotKey); wr(fp, buf);
	wsprintfW(buf, L"c\n\n"); wr(fp, buf);

	for(i = 0, sp = kbd->dpStoreArray; i < kbd->cxStoreArray; i++, sp++)
	{
		if(sp->dwSystemID > 0 && (sp->dwSystemID > TSS__MAX || !StoreTokens[sp->dwSystemID][0]))   // I4652
			wsprintfW(buf, L"c store(&%d) %s\n", sp->dwSystemID, ExtString(sp->dpString));
		else
			wsprintfW(buf, L"store(%s) %s\n", storename(i+1), ExtString(sp->dpString));

    if(sp->dwSystemID == TSS_BITMAP)
    {
      WideCharToMultiByte(CP_ACP, 0, sp->dpString, -1, bmpbuf, _MAX_PATH, NULL, NULL);
      _splitpath_s(bmpbuf, NULL, NULL, NULL, NULL, bmp_filename, _MAX_FNAME, bmp_ext, _MAX_EXT);
      _splitpath_s(bmpfile, bmp_drive, _MAX_DRIVE, bmp_dir, _MAX_DIR, NULL, NULL, NULL, NULL);
      _makepath_s(bmpbuf, _MAX_PATH, bmp_drive, bmp_dir, bmp_filename, bmp_ext);
      bmpfile = bmpbuf;
    }
		wr(fp, buf);
	}

	wsprintfW(buf, L"\n"); wr(fp, buf);

	if(kbd->StartGroup[0] != -1)
	{
		wsprintfW(buf, L"begin ANSI    > use(%s)\n", groupname(kbd->StartGroup[0]+1)); wr(fp, buf);
	}
	if(kbd->StartGroup[1] != -1)
	{
		wsprintfW(buf, L"begin Unicode > use(%s)\n", groupname(kbd->StartGroup[1]+1)); wr(fp, buf);
	}

	wsprintfW(buf, L"\n"); wr(fp, buf);

	for(i = 0, gp = kbd->dpGroupArray; i < kbd->cxGroupArray; i++, gp++)
	{
		wsprintfW(buf, L"group(%s)%s\n", groupname(i+1), gp->fUsingKeys ? L" using keys" : L""); wr(fp, buf);
		for(j = 0, kp = gp->dpKeyArray; j < gp->cxKeyArray; j++, kp++)
		{
			wsprintfW(buf, L"  %s%s%s> %s c line(%d)\n", ExtString(kp->dpContext), gp->fUsingKeys ? L" + " : L"",
        gp->fUsingKeys ? KeyString(kp) : L" ", ExtString(kp->dpOutput), kp->Line);
			wr(fp, buf);
		}

		if(gp->dpMatch)
		{
			wsprintfW(buf, L"  match > %s\n", ExtString(gp->dpMatch));
			wr(fp, buf);
		}

		if(gp->dpNoMatch)
		{
			wsprintfW(buf, L"  nomatch > %s\n", ExtString(gp->dpNoMatch));
			wr(fp, buf);
		}

		wsprintfW(buf, L"\n"); wr(fp, buf);
	}

	wsprintfW(buf, L"c EOF\n\n"); wr(fp, buf);
	fclose(fp);

  if (lpBitmap && cbBitmap && bmpfile && *bmpfile) {
    SaveBitmapFile(lpBitmap, cbBitmap, bmpfile);
  }

	return 0;
}

void SaveBitmapFile(LPBYTE lpBitmap, DWORD cbBitmap, char *bmpfile)
{
  FILE *fp;
  if (fopen_s(&fp, bmpfile, "wb") == 0) {
    fwrite(lpBitmap, cbBitmap, 1, fp);
    fclose(fp);
  }
}
