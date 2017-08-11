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


#define STRICT
#include <windows.h>
#include <stdio.h>

#define _KEYMAN64_LIGHT

#include "keyman64.h"
//#include "keymanedition.h"
#include "vkeys.h"

#define SSN__PREFIX		L"&"

const char *UTF16Sig = "\xFF\xFE";

extern void Err(char *p);
void SaveBitmapFile(LPBYTE lpBitmap, DWORD cbBitmap, char *bmpfile);

LPKEYBOARD g_kbd;

PWCHAR wcscat2(PWCHAR c1, const PWCHAR c2)
{
	wcscat(c1, c2);
	return wcschr(c1, 0);
}

#define MAX_SYSTEM_STORE 41   // I4652
const PWCHAR StoreTokens[MAX_SYSTEM_STORE] = {   // I4652
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
	NULL
};

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
	if(flag & LCTRLFLAG)       wcscat2(buf, L"LCTRL ");
	if(flag & RCTRLFLAG)       wcscat2(buf, L"RCTRL ");
	if(flag & LALTFLAG)        wcscat2(buf, L"LALT ");
	if(flag & RALTFLAG)        wcscat2(buf, L"RALT ");
	if(flag & K_SHIFTFLAG)     wcscat2(buf, L"SHIFT ");
	if(flag & K_CTRLFLAG)      wcscat2(buf, L"CTRL ");
	if(flag & K_ALTFLAG)       wcscat2(buf, L"ALT ");
	if(flag & CAPITALFLAG)     wcscat2(buf, L"CAPS ");
	if(flag & NOTCAPITALFLAG)  wcscat2(buf, L"NCAPS ");
	if(flag & NUMLOCKFLAG)     wcscat2(buf, L"NUM ");
	if(flag & NOTNUMLOCKFLAG)  wcscat2(buf, L"NNUM ");
	if(flag & SCROLLFLAG)      wcscat2(buf, L"SCROLL ");
	if(flag & NOTSCROLLFLAG)   wcscat2(buf, L"NSCROLL ");
	return buf;
}

PWCHAR GetVKeyName(LPKEY key)  // I3438
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
  if(ch == L'1') return L"!=";
  return L"=";
}

PWCHAR ExtString(PWCHAR str)
{
	static WCHAR buf[2][512], bufpointer = 0;	// allows for multiple strings in one printf
												// dodgy hack?

	PWCHAR p;
	int inquotes = 0;

	bufpointer = !bufpointer;
	p = buf[bufpointer]; *p = 0;

	for(; *str; str++)
	{
		if(*str == UC_SENTINEL)
		{
			str++;
			if(inquotes) p = wcscat2(p, L"\" ");
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
				p = wcscat2(p, L"context ");
				break;
			case CODE_NUL:
				p = wcscat2(p, L"nul ");
				break;
			case CODE_USE:
				str++;
				wsprintfW(p, L"use(%s) ", groupname(*str));
				p = wcschr(p, 0);
				break;
			case CODE_RETURN:
				p = wcscat2(p, L"return ");
				break;
			case CODE_BEEP:
				p = wcscat2(p, L"beep ");
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
						str+1, *(str+1) == L'"' ? L'\'' : L'"');
				else
        {
          if(*(str+1) > VK__MAX)  // I3438
            //TODO: Get extended key value
            wsprintfW(p, L"[%s %d] ", flagstr(*str), *(str+1)-VK__MAX-1);
          else
					  wsprintfW(p, L"[%s%s] ", flagstr(*str), VKeyNames[*(str+1)]);
        }
				str++;
				p = wcschr(p, 0);
				break;
			case CODE_SWITCH:
				p = wcscat2(p, L"switch<deprecated> ");
				break;
			case CODE_KEY:
				p = wcscat2(p, L"key<deprecated> ");
				break;
			case CODE_CLEARCONTEXT:
				p = wcscat2(p, L"clearcontext ");
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
				p = wcscat2(p, L"unknown() ");
				break;
			}
		}
		else
		{
			if(*str == L'"')
			{
				if(inquotes) p = wcscat2(p, L"\" ");
				inquotes = 0;
				p = wcscat2(p, L"'\"' ");
			}
			else if(*str < 32)
			{
				if(inquotes) p = wcscat2(p, L"\" ");
				inquotes = 0;
				wsprintfW(p, L"x%x ", *str);
				p = wcschr(p, 0);
			}
			else
			{
				if(!inquotes)
				{
					inquotes = 1;
					p = wcscat2(p, L"\"");
				}
				*p++ = *str;
				*p = 0;
			}
		}
	}

	if(inquotes) p = wcscat2(p, L"\" ");
	return buf[bufpointer];
}

void wr(FILE *fp, PWSTR buf)
{
	fwrite(buf, wcslen(buf) * 2, 1, fp);
}

int SaveKeyboardSource(LPKEYBOARD kbd, LPBYTE lpBitmap, DWORD cbBitmap, char *filename, char *bmpfile)
{
	PWCHAR buf;
	FILE *fp;
	LPSTORE sp;
	LPGROUP gp;
	LPKEY kp;
	unsigned int i, j;
  char bmpbuf[256];

	buf = new WCHAR[1024];

	g_kbd = kbd;

	fp = fopen(filename, "wb");
	fwrite(UTF16Sig, 2, 1, fp);

	if(!fp) { Err("Unable to create output file."); return 3; }

	wsprintfW(buf, L"c Keyboard created by KMDECOMP\n"); wr(fp, buf);
	wsprintfW(buf, L"c\n"); wr(fp, buf);
	wsprintfW(buf, L"c Meta details: Registered=%d; Version=%x\n", kbd->IsRegistered, kbd->version); wr(fp, buf);
	wsprintfW(buf, L"c               Flags=%x; HotKey=%x\n", kbd->dwFlags, kbd->dwHotKey); wr(fp, buf);
	wsprintfW(buf, L"c\n\n"); wr(fp, buf);

	for(i = 0, sp = kbd->dpStoreArray; i < kbd->cxStoreArray; i++, sp++)
	{
		if(sp->dwSystemID > 0 && (sp->dwSystemID >= MAX_SYSTEM_STORE || !StoreTokens[sp->dwSystemID][0]))   // I4652
			wsprintfW(buf, L"c store(&%d) %s\n", sp->dwSystemID, ExtString(sp->dpString));
		else
			wsprintfW(buf, L"store(%s) %s\n", storename(i+1), ExtString(sp->dpString));

    if(sp->dwSystemID == TSS_BITMAP)
    {
      WideCharToMultiByte(CP_ACP, 0, sp->dpString, -1, bmpbuf, 256, NULL, NULL);
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

	SaveBitmapFile(lpBitmap, cbBitmap, bmpfile);

	return 0;
}

void SaveBitmapFile(LPBYTE lpBitmap, DWORD cbBitmap, char *bmpfile)
{
  FILE * fp = fopen(bmpfile, "wb");
  fwrite(lpBitmap, cbBitmap, 1, fp);
  fclose(fp);
}
