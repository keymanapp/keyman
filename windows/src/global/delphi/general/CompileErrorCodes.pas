(*
  Name:             CompileErrorCodes
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      6 Mar 2014

  Modified Date:    24 Aug 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          06 Mar 2014 - mcdurdin - I4118 - V9.0 - KMW compiler should warn when extended shift flags are used
                    19 Mar 2014 - mcdurdin - I4141 - V9.0 - Warn when unusable key ids are used
                    19 Mar 2014 - mcdurdin - I4142 - V9.0 - Validate key ids are in an acceptable format
                    04 May 2015 - mcdurdin - I4688 - V9.0 - Add build path to project settings
                    24 Aug 2015 - mcdurdin - I4872 - OSK font and Touch Layout font should be the same in Developer
*)
unit CompileErrorCodes;

interface

const
  CERR_NotSupportedInKeymanWebContext =               $4054;
  CERR_NotSupportedInKeymanWebOutput =                $4055;
  CERR_NotSupportedInKeymanWebStore =                 $4056;
  CERR_VirtualCharacterKeysNotSupportedInKeymanWeb =  $4057;
  CERR_VirtualKeysNotValidForMnemonicLayouts =        $4058;
  CERR_InvalidTouchLayoutFile =                       $4059;
  CERR_TouchLayoutInvalidIdentifier =                 $405A;   // I4142
  CERR_InvalidKeyCode =                               $405B;   // I4142

  CWARN_TouchLayoutMissingLayer =         $2091;
  CWARN_TouchLayoutCustomKeyNotDefined =  $2092;
  CWARN_TouchLayoutMissingRequiredKeys =  $2093;
  CWARN_HelpFileMissing =                 $2094;
  CWARN_EmbedJsFileMissing =              $2095;
  CWARN_TouchLayoutFileMissing =          $2096;
  CWARN_VisualKeyboardFileMissing =       $2097;
  CWARN_ExtendedShiftFlagsNotSupportedInKeymanWeb = $2098;   // I4118
  CWARN_TouchLayoutUnidentifiedKey =      $2099;   // I4142
  CWARN_UnreachableKeyCode =              $209A;   // I4141

  CWARN_CouldNotCopyJsonFile =            $209B;   // I4688

  CWARN_TouchLayoutFontShouldBeSameForAllPlatforms = $209F;   // I4872
  CWARN_InvalidJSONMetadataFile =         $20A0;   // I4872
  CWARN_JSONMetadataOSKFontShouldMatchTouchFont = $20A1;   // I4872

implementation

end.
