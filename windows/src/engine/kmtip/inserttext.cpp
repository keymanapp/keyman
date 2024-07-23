/*
  Name:             inserttext
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      19 Jun 2007

  Modified Date:    28 Mar 2016
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          19 Jun 2007 - mcdurdin - I890 - Fix deadkeys in TSF
                    19 Jun 2007 - mcdurdin - I890 - Fix crash with deadkeys - logging
                    11 Dec 2009 - mcdurdin - Header files
                    22 Mar 2010 - mcdurdin - Compiler fixup
                    24 Oct 2012 - mcdurdin - I3481 - V9.0 - Eliminate unsafe calls in C++
                    17 Nov 2012 - mcdurdin - I3564 - V9.0 - Add 'pseudo' mapping of characters to indicate kmtip is running
                    17 Nov 2012 - mcdurdin - I3565 - V9.0 - Add extra validation and logging to GetLeftOfSelection
                    01 Dec 2012 - mcdurdin - I3607 - V9.0 - Disable 'pseudo' mode in kmtip except for debugging
                    10 Jun 2014 - mcdurdin - I4262 - V9.0 - TSF deadkeys do not function correctly
                    28 Mar 2016 - mcdurdin - I4933 - Compat issue with Firefox 42 and IE and Keyman 9 TSF
*/
#include "pch.h"   // I4262
#include "kmtip.h"

//+---------------------------------------------------------------------------
//
// InsertTextAtSelection
//
//----------------------------------------------------------------------------

#ifdef DEBUG_PSEUDO   // I3607

WCHAR PseudoMap[128] =    // I3564
{
  0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,
  0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0x1A,0x1B,0x1C,0x1D,0x1E,0x1F,
  0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,0x28,0x29,0x2A,0x2B,0x2C,0x2D,0x2E,0x2F,
  0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x3A,0x3B,0x3C,0x3D,0x3E,0x3F,
  0x40,L'Ą',L'Ɓ',L'Ĉ',L'Đ',L'Ĕ',L'Ḟ',L'Ĝ',L'Ħ',L'Ĭ',L'Ĵ',L'Ķ',L'Ł',L'Ḿ',L'Ṉ',L'Ṏ',
  L'Ᵽ',L'Ꝙ',L'Ȑ',L'Ṧ',L'Ŧ',L'Ʉ',L'Ṽ',L'Ẃ',L'Ẍ',L'Ɏ',L'Ẕ', 0x5B,0x5C,0x5D,0x5E,0x5F,
  0x60,L'ⱥ',L'ḇ',L'ḉ',L'ḓ',L'ḕ',L'ḟ',L'ḡ',L'ḩ',L'ḭ',L'ɉ',L'ḵ',L'ƚ',L'ᴟ',L'ᴝ',L'ȭ',
  L'ᵽ',L'ʠ',L'ɍ',L'ȿ',L'ŧ',L'ů',L'ⱴ',L'ŵ',L'ᶍ',L'ɏ',L'ƶ', 0x7B,0x7C,0x7D,0x7E,0x7F
  //0x40,'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
  //'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 0x5B,0x5C,0x5D,0x5E,0x5F,
  //0x60,'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
  //'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 0x7B,0x7C,0x7D,0x7E,0x7F
};

void Pseudofy(WCHAR *pchText)   // I3564
{
  for(; *pchText; pchText++)
  {
    if(*pchText < 0x80) *pchText = PseudoMap[*pchText];
  }
}

void DePseudofy(WCHAR *pchText)   // I3564
{
  for(; *pchText; pchText++)
  {
    for(int k = 0; k < 0x80; k++)
    {
      if(*pchText == PseudoMap[k])
      {
        *pchText = k;
        break;
      }
    }
  }
}
#endif   // I3607

void InsertTextAtSelection(TfEditCookie ec, ITfContext *pContext, const WCHAR *pchText, ULONG cchText)
{
  LogEnter();

  ITfInsertAtSelection *pInsertAtSelection;
    ITfRange *pRange;
    TF_SELECTION tfSelection;

    // we need a special interface to insert text at the selection
    if (pContext->QueryInterface(IID_ITfInsertAtSelection, (void **)&pInsertAtSelection) != S_OK)
        return;

#ifdef DEBUG_PSEUDO   // I3607
    WCHAR *PseudoBuf = new WCHAR[cchText+1];   // I3564

    wcsncpy_s(PseudoBuf, cchText+1, pchText, cchText);
    PseudoBuf[cchText] = 0;
    Pseudofy(PseudoBuf);

    // insert the text
    if (pInsertAtSelection->InsertTextAtSelection(ec, 0, PseudoBuf, cchText, &pRange) != S_OK)   // I3564
        goto Exit;
#else   // I3607
    // insert the text
    if (pInsertAtSelection->InsertTextAtSelection(ec, 0, pchText, cchText, &pRange) != S_OK)   // I3564
        goto Exit;
#endif

    // update the selection, we'll make it an insertion point just past
    // the inserted text.
    pRange->Collapse(ec, TF_ANCHOR_END);

    tfSelection.range = pRange;
    tfSelection.style.ase = TF_AE_NONE;
    tfSelection.style.fInterimChar = FALSE;

    pContext->SetSelection(ec, 1, &tfSelection);

    pRange->Release();

Exit:

#ifdef DEBUG_PSEUDO   // I3607
    delete PseudoBuf;   // I3564
#endif   // I3607

    pInsertAtSelection->Release();
}

void DeleteLeftOfSelection(TfEditCookie ec, ITfContext *pContext, LONG n)
{
  LogEnter();
  TF_SELECTION tfSelection;
	ULONG cFetched;
	LONG outn;

  if(pContext->GetSelection(ec, TF_DEFAULT_SELECTION, 1, &tfSelection, &cFetched) != S_OK || cFetched == 0)
		return;

  //TODO: log failures
	if(tfSelection.range->ShiftStart(ec, -n, &outn, NULL) != S_OK) {
    tfSelection.range->Release();
    return;
  }

  WCHAR buf[2] = L"";
	if(tfSelection.range->SetText(ec, 0, buf, 0) != S_OK) {
    tfSelection.range->Release();
    return;
  }

  // release the range
  tfSelection.range->Release();
}

char *debugstr(PWSTR buf) {
	WCHAR *p;
	char *bufout = new char[20*7+1];
	char *q;
  *bufout = 0;
	for(p = buf, q = bufout; *p && (p-buf < 20); p++)
	{
		wsprintf(q, "U+%4.4X ", *p); q = strchr(q, 0);
	}
	//WideCharToMultiByte(CP_ACP, 0, buf, -1, bufout, 128, NULL, NULL);
	return bufout;
}

BOOL
GetLeftOfSelection(
  TfEditCookie ec,
  ITfContext *pContext,
  WCHAR *buf,
  LONG n,
  HRESULT *hrGetSelection,
  ULONG *cFetched,
  TF_SELECTION *tfSelection)  // I4933
{
  LogEnter();

  TF_SELECTION tfSelectionLocal = {0};
  ULONG cFetchedLocal;
	LONG outn;
	ITfRange *pRange, *pRangeEnd;
  HRESULT hr;

  /*   // I4933
    First we will try to see if there is any text in the control, and if not, then treat
    it as transitory (that is, no ability to read context. This seems to happen in Firefox
    with RICHEDIT controls - for example, SourceForge comment fields e.g. reported on page
    https://sourceforge.net/p/greekpolytonicsp/discussion/general/thread/9b6fa46d/
    This also happens in Internet Explorer in the same fields. It is unclear at this point
    if there is the same root cause or if Firefox just happens to be copying IE behaviour.
  */

  if(!SUCCEEDED(hr = pContext->GetStart(ec, &pRange))) {
    Log(L"GetLeftOfSelection: Exit -- Failed GetStart = %x", hr);   // I3565
		return FALSE;
	}

  if(!SUCCEEDED(hr = pContext->GetEnd(ec, &pRangeEnd))) {
    Log(L"GetLeftOfSelection: Exit -- Failed GetEnd = %x", hr);   // I3565
    pRange->Release(); pRange = NULL;
		return FALSE;
	}

  if(!SUCCEEDED(hr = pRange->ShiftEndToRange(ec, pRangeEnd, TF_ANCHOR_END))) {
    Log(L"GetLeftOfSelection: Exit -- Failed ShiftEndToRange = %x", hr);
    pRange->Release(); pRange = NULL;
    pRangeEnd->Release(); pRangeEnd = NULL;
    return FALSE;
  }

  BOOL bTreatAsTransitory = FALSE;
  if(!SUCCEEDED(hr = pRange->GetText(ec, 0, buf, n, &cFetchedLocal))) {
    Log(L"GetLeftOfSelection: Exit -- Failed GetRange (all text to 63 chars) = %x", hr);
    bTreatAsTransitory = TRUE;
  } else if(cFetchedLocal == 0) {
    Log(L"GetLeftOfSelection: Exit -- no text in edit control, treating as transitory");
    bTreatAsTransitory = TRUE;
  }

  pRange->Release(); pRange = NULL;
  pRangeEnd->Release(); pRangeEnd = NULL;

  if(bTreatAsTransitory) {
    buf[0] = 0;
    return FALSE;
  }

  /*
    At this point, we know we can read content from the edit control, so we just need
    to read the range to the left of the selection - up to (n) characters.
  */

  if (!SUCCEEDED(hr = *hrGetSelection = pContext->GetSelection(ec, TF_DEFAULT_SELECTION, 1, tfSelection, cFetched)))  // I3565
	{
		Log(L"GetLeftOfSelection: Exit -- Failed GetSelection = %x", hr);   // I3565
		return FALSE;
	}

  // copy the values for local processing, preserving the function call variables
  cFetchedLocal    = *cFetched;
  tfSelectionLocal = *tfSelection;
  if (tfSelectionLocal.range) {
    tfSelectionLocal.range->AddRef();
  }

  if(cFetchedLocal == 0)   // I3565
  {
    Log(L"GetLeftOfSelection: Exit -- cFetchedLocal == 0");   // I3565
    if(tfSelectionLocal.range != NULL)
      tfSelectionLocal.range->Release();
    return FALSE;
  }

	if(!SUCCEEDED(hr = tfSelectionLocal.range->Clone(&pRange)))   // I3565
  {
    Log(L"GetLeftOfSelection: Failed range->Clone = %x", hr);   // I3565
    tfSelectionLocal.range->Release();
    return FALSE;
  }

	if(!SUCCEEDED(hr = pRange->Collapse(ec, TF_ANCHOR_START)))   // I3565
  {
    Log(L"GetLeftOfSelection: Failed range->Collapse = %x", hr);   // I3565
    tfSelectionLocal.range->Release();
    pRange->Release();
    return FALSE;
  }
	if(!SUCCEEDED(hr = pRange->ShiftStart(ec, -n, &outn, NULL)))   // I3565
  {
    Log(L"GetLeftOfSelection: Failed range->ShiftStart = %x", hr);   // I3565
    tfSelectionLocal.range->Release();
    pRange->Release();
    return FALSE;
  }

  BOOL result = TRUE;

	if(SUCCEEDED(hr = pRange->GetText(ec, 0, buf, n, &cFetchedLocal)))   // I3565
  {
		buf[cFetchedLocal] = 0;
    if(ShouldDebug()) {
      char *p = debugstr(buf);
      Log(L"GetLeftOfSelection(%d) = %hs [%d fetched]", n, p, cFetchedLocal);
      delete[] p;
    }
#ifdef DEBUG_PSEUDO   // I3607
    DePseudofy(buf);   // I3564
#endif   // I3607
  }
	else
  {
    Log(L"GetLeftOfSelection: Failed range->GetText = %x", hr);   // I3565
    buf[0] = 0;
    result = FALSE;
  }

	pRange->Release();
  tfSelectionLocal.range->Release();

  return result;   // I4933
}
