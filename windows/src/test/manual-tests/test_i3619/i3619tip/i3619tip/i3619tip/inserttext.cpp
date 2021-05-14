/*
  Name:             inserttext
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      19 Jun 2007

  Modified Date:    1 Dec 2012
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
*/
#include "stdafx.h"

//+---------------------------------------------------------------------------
//
// InsertTextAtSelection
//
//----------------------------------------------------------------------------

void InsertTextAtSelection(TfEditCookie ec, ITfContext *pContext, const WCHAR *pchText, ULONG cchText)
{
    ITfInsertAtSelection *pInsertAtSelection;
    ITfRange *pRange;
    TF_SELECTION tfSelection;

    // we need a special interface to insert text at the selection
    if (pContext->QueryInterface(IID_ITfInsertAtSelection, (void **)&pInsertAtSelection) != S_OK)
        return;

    // insert the text
    if (pInsertAtSelection->InsertTextAtSelection(ec, 0, pchText, cchText, &pRange) != S_OK)   // I3564
        goto Exit;

    // update the selection, we'll make it an insertion point just past
    // the inserted text.
    pRange->Collapse(ec, TF_ANCHOR_END);

    tfSelection.range = pRange;
    tfSelection.style.ase = TF_AE_NONE;
    tfSelection.style.fInterimChar = FALSE;

    pContext->SetSelection(ec, 1, &tfSelection);

    pRange->Release();

Exit:

    pInsertAtSelection->Release();
}

