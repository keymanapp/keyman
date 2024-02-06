
#include "stdafx.h"
#include <stdio.h>
#include <initguid.h>
#include <codecvt>
#include <locale>
#include <string>

#define SESSION_NAME  TEXT("Keyman-Debug-ETWProvider")

// {DA621615-E08B-4283-918E-D2502D3757AE}
DEFINE_GUID(
  g_Provider,
  0xDA621615, 0xE08B, 0x4283, 0x91, 0x8E, 0xD2, 0x50, 0x2D, 0x37, 0x57, 0xAE);

void WINAPI EventRecordCallback(PEVENT_RECORD event);
BOOL InsertRecord(LONGLONG event_id, LONGLONG timestamp, ULONG pid, ULONG tid,
  PEVENT_RECORD event, PTRACE_EVENT_INFO info);

TRACEHANDLE hTrace = INVALID_PROCESSTRACE_HANDLE;
LONGLONG recordNum = 0;
FILE *fp = NULL;
PTRACE_EVENT_INFO pInfo = NULL;
DWORD pInfoBufferSize = 0;

BOOL LoadTrace(PTSTR szPath) {
  EVENT_TRACE_LOGFILE trace = { 0 };
  TCHAR session[] = SESSION_NAME;

  if (szPath != NULL) {
    trace.LogFileName = szPath;
    trace.ProcessTraceMode = PROCESS_TRACE_MODE_EVENT_RECORD;
  }
  else {
    trace.LoggerName = session;
    trace.ProcessTraceMode = PROCESS_TRACE_MODE_EVENT_RECORD | PROCESS_TRACE_MODE_REAL_TIME;
  }

  trace.EventRecordCallback = EventRecordCallback;

  hTrace = OpenTrace(&trace);
  if (hTrace == INVALID_PROCESSTRACE_HANDLE) {
    DoLogError(L"OpenTrace failed with error %d", GetLastError());
    return FALSE;
  }

  return TRUE;
}

BOOL CreateLogFile(PTSTR szOutputFile) {
  _wfopen_s(&fp, szOutputFile, L"w");

  fwprintf(fp,
    L"Index\t"
    L"Timestamp\t"
    L"Platform\t"
    L"Process\t"
    L"PID\t"
    L"TID\t"
    L"ModifierState\t"
    L"ModifierState\t"
    L"ActualModifierState\t"
    L"ActualModifierState\t"
    L"TickCount\t"
    L"FocusHWND\t"
    L"ActiveHKL\t"
    L"SourceFile\t"
    L"SourceLine\t"
    L"Message\n");

  return TRUE;
}

BOOL Convert(PTSTR szPath, PTSTR szOutputFile) {
  DWORD status;
  int repeat = 0;
  if (!LoadTrace(szPath)) return FALSE;
  if (!CreateLogFile(szOutputFile)) return FALSE;

  DoLogInfo(L"Starting to process trace from %s into %s", szPath, szOutputFile);
  do {
    status = ProcessTrace(&hTrace, 1, NULL, NULL);
    if (status != ERROR_SUCCESS) {
      DoLogError(L"ProcessTrace is not ready (received error %d). Waiting 0.1 seconds", status);
      Sleep(100);
      if (++repeat > 20) break;
    }
  } while (status != ERROR_SUCCESS);

  CloseTrace(hTrace);

  fclose(fp);

  return TRUE;
}

void WINAPI EventRecordCallback(PEVENT_RECORD event) {
  if (!IsEqualGUID(event->EventHeader.ProviderId, g_Provider)) {
    return;
  }

  if (++recordNum % 10000 == 0) {
    // This stops our buffers growing too large, and in theory
    // in the future allows us to watch the trace in "real time"
    DoLogInfo(L"... %lld records", recordNum);
  }

  DWORD status = TdhGetEventInformation(event, 0, NULL, pInfo, &pInfoBufferSize);
  if (status == ERROR_INSUFFICIENT_BUFFER) {
    if (pInfo) free(pInfo);
    pInfo = (TRACE_EVENT_INFO *)malloc(pInfoBufferSize);
    status = TdhGetEventInformation(event, 0, NULL, pInfo, &pInfoBufferSize);
  }

  if (status != ERROR_SUCCESS) {
    DoLogError(L"Failed to get event information with error %d", status);
    return;
  }

  //if (pInfo->TaskNameOffset == 0) {
  //  DoLogError(L"Event has no task name");
//    return;
//  }

  // TODO: this is calling out for refactoring ... ...
  //PWSTR pTask = (PWSTR)((PBYTE)(pInfo)+pInfo->TaskNameOffset);
  //if (wcscmp(pTask, L"Message") == 0) {

    /*
      < data name = "Platform" inType = "win:UInt32" map = "Platform" / >
    <data name = "Process" inType = "win:UnicodeString" / >
    <data name = "PID" inType = "win:UInt32" / >
    <data name = "TID" inType = "win:UInt32" / >
    <data name = "ModifierState" inType = "win:UInt32" map = "ModifierState" / >
    <data name = "ActualModifierState" inType = "win:UInt32" map = "ModifierState" / >
    <data name = "TickCount" inType = "win:UInt32" / >
    <data name = "FocusHWND" inType = "win:UInt32" / >
    <data name = "ActiveHKL" inType = "win:UInt32" / >
    <data name = "SourceFile" inType = "win:UnicodeString" / >
    <data name = "SourceLine" inType = "win:UInt32" / >
    <data name = "Message" inType = "win:UnicodeString" / >


*/


    if (!InsertRecord(recordNum,
        event->EventHeader.TimeStamp.QuadPart,
        event->EventHeader.ProcessId,
        event->EventHeader.ThreadId,
        event,
        pInfo)) {
      DoLogError(L"Failed to insert Event record for Message");
      return;
    }
  //}
  //else {
  //  DoLogError(L"Received an event with an unexpected name '%s'", pTask);
  //  return;
  //}
}


PBYTE propertyBuf = NULL;
ULONG currentPropertyBufSize = 0, propertyBufSize = 0;

BOOL GetEventProperties(PEVENT_RECORD event, PTRACE_EVENT_INFO info, int propertyIndex, PWSTR &propertyName) {
  propertyName = (PWCHAR)((PBYTE)(pInfo)+pInfo->EventPropertyInfoArray[propertyIndex].NameOffset);
  PROPERTY_DATA_DESCRIPTOR pdd = { 0 };
  pdd.ArrayIndex = ULONG_MAX;
  pdd.PropertyName = (ULONGLONG)(propertyName);

  ULONG sz;
  DWORD status = TdhGetPropertySize(event, 0, NULL, 1, &pdd, &sz);
  if (status != ERROR_SUCCESS) {
    DoLogError(L"Failed to get property size with error %d", status);
    return FALSE;
  }

  if (sz == 0) {
    return TRUE;
  }
  else {

    if (sz + 2 > propertyBufSize) {
      if (propertyBuf) delete propertyBuf;
      propertyBufSize = sz + 2; // add trailing WCHAR nul
      propertyBuf = new BYTE[propertyBufSize];
    }
    status = TdhGetProperty(event, 0, NULL, 1, &pdd, propertyBufSize, propertyBuf);
    // Always nul terminate (even if not a string!)
    propertyBuf[sz] = 0;
    propertyBuf[sz + 1] = 0;
    if (status != ERROR_SUCCESS) {
      DoLogError(L"Failed to get property with error %d", status);
      return FALSE;
    }
    return TRUE;
  }
}

void PrintPlatform(int platform) {
  switch (platform) {
  case 1: fwprintf(fp, L"\tx86"); break;
  case 2: fwprintf(fp, L"\tx64"); break;
  default: fwprintf(fp, L"\t%d", platform); break;
  }
}

#define LCTRLFLAG		0x0001		// Left Control flag
#define RCTRLFLAG		0x0002		// Right Control flag
#define LALTFLAG		0x0004		// Left Alt flag
#define RALTFLAG		0x0008		// Right Alt flag
#define K_SHIFTFLAG		0x0010		// Either shift flag
#define K_CTRLFLAG		0x0020		// Either ctrl flag
#define K_ALTFLAG		0x0040		// Either alt flag
#define CAPITALFLAG		0x0100		// Caps lock on
#define NOTCAPITALFLAG	0x0200		// Caps lock NOT on
#define NUMLOCKFLAG		0x0400		// Num lock on
#define NOTNUMLOCKFLAG	0x0800		// Num lock NOT on
#define SCROLLFLAG		0x1000		// Scroll lock on
#define NOTSCROLLFLAG	0x2000		// Scroll lock NOT on
#define ISVIRTUALKEY	0x4000		// It is a Virtual Key Sequence
#define VIRTUALCHARKEY	0x8000		// Keyman 6.0: Virtual Key Cap Sequence NOT YET

void PrintModifierState(int state) {
  fwprintf(fp, L"\t%x\t", state);
  if (state == 0) {
    fwprintf(fp, L"-");
    return;
  }

  if (state & LCTRLFLAG) fwprintf(fp, L"LCTRL ");
  if (state & RCTRLFLAG) fwprintf(fp, L"RCTRL ");
  if (state & LALTFLAG) fwprintf(fp, L"LALT ");
  if (state & RALTFLAG) fwprintf(fp, L"RALT ");
  if (state & K_SHIFTFLAG) fwprintf(fp, L"SHIFT ");
  if (state & K_CTRLFLAG) fwprintf(fp, L"CTRL ");
  if (state & K_ALTFLAG) fwprintf(fp, L"ALT ");
  if (state & CAPITALFLAG) fwprintf(fp, L"CAPS ");
  if (state & NOTCAPITALFLAG) fwprintf(fp, L"NCAPS ");
  if (state & NUMLOCKFLAG) fwprintf(fp, L"NUM ");
  if (state & NOTNUMLOCKFLAG) fwprintf(fp, L"NNUM ");
  if (state & SCROLLFLAG) fwprintf(fp, L"SCROLL ");
  if (state & NOTSCROLLFLAG) fwprintf(fp, L"NSCROLL ");
}

void PrintHWND(HWND hwnd) {
  fwprintf(fp, L"\t%x", (int) hwnd);
}

void PrintHKL(HKL hkl) {
  fwprintf(fp, L"\t%08.8x", (int) hkl);
}

std::string string_from_u16string(std::u16string const str) {
	std::wstring_convert<std::codecvt_utf8_utf16<char16_t>, char16_t> converter;
	return converter.to_bytes(str);
}

void PrintDefault(PWSTR propertyName, int type) {
  switch (type) {
  case TDH_INTYPE_UNICODESTRING:
    {
      std::u16string utf16buf((char16_t*)propertyBuf);
      auto utf8buf = string_from_u16string(utf16buf);
      fprintf(fp, "\t%s", utf8buf.c_str());
    }
    break;
  case TDH_INTYPE_ANSISTRING:
    fwprintf(fp, L"\t%hs", (PCHAR)propertyBuf);
    break;
  case TDH_INTYPE_INT16:
  case TDH_INTYPE_UINT16:
    fwprintf(fp, L"\t%d", *(short *)propertyBuf);
    break;
  case TDH_INTYPE_INT32:
  case TDH_INTYPE_UINT32:
    fwprintf(fp, L"\t%d", *(int *)propertyBuf);
    break;
  case TDH_INTYPE_INT64:
  case TDH_INTYPE_UINT64:
    fwprintf(fp, L"\t%lld", *(LONGLONG *)propertyBuf);
    break;
  case TDH_INTYPE_BINARY:
    // TODO?
    break;
  default:
    DoLogError(L"Table has a property %s with unexpected type %d", propertyName, type);
    //return FALSE;
  }
}

BOOL InsertRecord(LONGLONG event_id, LONGLONG timestamp, ULONG pid, ULONG tid,
  PEVENT_RECORD event, PTRACE_EVENT_INFO info) {

  fwprintf(fp,
    L"%lld\t"
    L"%lld",
    event_id,
    timestamp);

  for (ULONG i = 0, col = 0; i < pInfo->TopLevelPropertyCount; i++) {
    PWSTR propertyName;
    if (!GetEventProperties(event, info, i, propertyName)) return FALSE;

    PWCHAR pdot = wcsstr(propertyName, L".Length");
    if (pdot) {
      // We assume this is a property for the length of the next field
      // We'll use the field but we won't be inserting into the database
      currentPropertyBufSize = *(int *)propertyBuf;
      continue;
    }

    // Special case column formatting
    switch (col) {
    case 0: PrintPlatform(*(int *)propertyBuf); break;
    case 4: PrintModifierState(*(int *)propertyBuf); break; // ModifierState
    case 5: PrintModifierState(*(int *)propertyBuf); break; // ActualModifierState
    case 7: PrintHWND(*(HWND *)propertyBuf); break; // FocusHWND
    case 8: PrintHKL(*(HKL *)propertyBuf); break; // ActiveHKL
    case 1: // process
    case 2: // PID
    case 3: // TID
    case 6: // TickCount
    case 9: // SourceFile
    case 10: // SourceLine
    case 11: // Message
    default:
      PrintDefault(propertyName, pInfo->EventPropertyInfoArray[i].nonStructType.InType);
      break;
    }


    col++;
  }

  fwprintf(fp, L"\n");
  return TRUE;
}

