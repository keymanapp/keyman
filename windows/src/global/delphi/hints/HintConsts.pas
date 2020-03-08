(*
  Name:             HintConsts
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      27 Mar 2008

  Modified Date:    27 Mar 2008
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          27 Mar 2008 - mcdurdin - I1248 - Initial version
*)
unit HintConsts;

interface

type
  TKeymanHint = (
    KH_NULL,
    KH_TUTORIALFINISHED,  // [OK]
    KH_EXITPRODUCT, // [OK] Cancel
    KH_CLOSEOSK,  // [OK]
    KH_OSKCLOSEDAUTOMATICALLY,   // [OK]
    KH_OSKHINTBAR
    );

  TKeymanHintData = record
    IsQuestion: Boolean; // True = OK+Cancel buttons; False = OK button only
  end;

const
  KeymanHintData: array[TKeymanHint] of TKeymanHintData = (
    (IsQuestion: False),
    (IsQuestion: False),
    (IsQuestion: True),
    (IsQuestion: False),
    (IsQuestion: False),
    (IsQuestion: False)
  );

implementation


end.
