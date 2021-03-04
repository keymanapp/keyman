(*
  Name:             kwhelp
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    9 Aug 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          28 Sep 2006 - mcdurdin - Support additional context tags, for context help
                    04 Jan 2007 - mcdurdin - Add notany keyword
                    30 Apr 2015 - mcdurdin - I4677 - V9.0 - Move Developer help to online only
                    09 Aug 2015 - mcdurdin - I4840 - Context-sensitive help missing V8 and V9 keywords
                    09 Aug 2015 - mcdurdin - I4841 - Restructure version 9 developer help
                    
*)
unit kwhelp;

interface

function IsValidHelpToken(var token: WideString; HelpOnSyntacticElements: Boolean): Boolean;

implementation

uses
  System.SysUtils,

  Keyman.Developer.System.HelpTopics;

type
  THelpArray = record
    Word: string;
    Topic: string;  // if not specified, then will default to "[guide|reference]/<word>"
    Guide: Boolean; // if true, uses SLanguageGuide else SLanguageReference
  end;

const
  HelpArray: array[0..74] of THelpArray = (   // I4840
    (Word: 'BITMAP' ),
    (Word: 'begin' ),
    (Word: 'COPYRIGHT' ),
    (Word: 'HOTKEY' ),
    (Word: 'LANGUAGE' ),
    (Word: 'LAYOUT' ),
    (Word: 'MESSAGE' ),
    (Word: 'NAME' ),
{9} (Word: 'VERSION' ),

    (Word: '&BITMAP'; Topic: 'bitmap' ),
    (Word: '&CasedKeys'; Topic: 'casedkeys' ),
    (Word: '&COPYRIGHT'; Topic: 'copyright' ),
    (Word: '&EthnologueCode'; Topic: 'ethnologuecode' ),   // I4840
    (Word: '&HOTKEY'; Topic: 'hotkey' ),
    (Word: '&includecodes'; Topic: 'includecodes' ),   // I4840
    (Word: '&KeyboardVersion'; Topic: 'keyboardversion' ),   // I4840
    (Word: '&kmw_embedcss'; Topic: 'kmw_embedcss' ),   // I4840
    (Word: '&kmw_embedjs'; Topic: 'kmw_embedjs' ),   // I4840
    (Word: '&kmw_helpfile'; Topic: 'kmw_helpfile' ),   // I4840
    (Word: '&kmw_helptext'; Topic: 'kmw_helptext' ),   // I4840
    (Word: '&kmw_rtl'; Topic: 'kmw_rtl' ),   // I4840
    (Word: '&LANGUAGE'; Topic: 'language' ),
    (Word: '&layer'; Topic: 'layer' ),   // I4840
    (Word: '&LAYOUT'; Topic: 'layout' ),
    (Word: '&LAYOUTFILE'; Topic: 'layoutfile' ),   // I4840
    (Word: '&MESSAGE'; Topic: 'message' ),
    (Word: '&Mnemoniclayout'; Topic: 'mnemoniclayout' ),   // I4840
    (Word: '&NAME'; Topic: 'name' ),
    (Word: '&oldcharposmatching'; Topic: 'oldcharposmatching' ),   // I4840
    (Word: '&targets'; Topic: 'targets' ),   // I4840
    (Word: '&VERSION'; Topic: 'version' ),
    (Word: '&visualkeyboard'; Topic: 'visualkeyboard' ),   // I4840
    (Word: '&windowslanguages'; Topic: 'windowslanguages' ),   // I4840
    (Word: '&capsononly'; Topic: 'caps' ),
    (Word: '&capsalwaysoff'; Topic: 'caps' ),
{26}(Word: '&shiftfreescaps'; Topic: 'caps' ),   // I4840

    (Word: 'any' ),
    (Word: 'baselayout' ),   // I4840
    (Word: 'beep' ),

    (Word: 'begin' ),
    (Word: 'ansi'; Topic: 'begin' ),
    (Word: 'unicode'; Topic: 'begin' ),

    (Word: 'context' ),
    (Word: 'deadkey' ),
    (Word: 'dk'; Topic: 'deadkey' ),

    (Word: 'group' ),
    (Word: 'using'; Topic: 'group' ),
    (Word: 'keys'; Topic: 'group' ),

    (Word: 'if' ),   // I4840
    (Word: 'index' ),
    (Word: 'match' ),
    (Word: 'nomatch' ),
    (Word: 'notany' ),   // I4840
    (Word: 'nul' ),
    (Word: 'outs' ),
    (Word: 'platform' ),   // I4840
    (Word: 'reset' ),   // I4840
    (Word: 'return' ),
    (Word: 'save' ),   // I4840
    (Word: 'set' ),   // I4840
    (Word: 'store' ),
    (Word: 'use' ),

    (Word: 'caps'; Topic: 'caps' ),
    (Word: 'on'; Topic: 'caps' ),
    (Word: 'only'; Topic: 'caps' ),
    (Word: 'always'; Topic: 'caps' ),
    (Word: 'off'; Topic: 'caps' ),
    (Word: 'shift'; Topic: 'caps' ),
{32}(Word: 'frees'; Topic: 'caps' ),

{1} (Word: 'c'; Topic: 'comments'; Guide: True ),

{3} (Word: '*virtual-key'; Topic: 'virtual-keys'; Guide: True ),
    (Word: '*character'; Topic: 'strings'; Guide: True ),
    (Word: '*string'; Topic: 'strings'; Guide: True ),
    (Word: '*constant'; Topic: 'constants'; Guide: True ),
    (Word: '*compile-target'; Topic: 'compile-targets'; Guide: True )
  );

function IsValidHelpToken(var token: WideString; HelpOnSyntacticElements: Boolean): Boolean;
var
  i: Integer;
begin
  if Copy(token,1,1) = '(' then
  begin
    Delete(token,1,1);
    Delete(token,Length(token),1);
    token := Trim(token);
  end;

  if (Length(token) > 2) and ((token[1] = 'c') or (token[1] = 'C')) and ((token[2] < #33) or (token[2] = #160)) then
  begin
    token := 'c';
  end
  else if HelpOnSyntacticElements then
  begin
    if Copy(token, 1, 1) = '[' then
    begin
      token := '*virtual-key';
    end
    else if SameText(Copy(token, 1, 2), 'U+') or SameText(Copy(token, 1, 1), 'd') or SameText(Copy(token, 1, 1), 'x') then
    begin
      token := '*character';
    end
    else if (token <> '') and CharInSet(token[1], ['''', '"']) then
    begin
      token := '*string';
    end
    else if Copy(token, 1, 1) = '$' then
    begin
      if SameText(token, '$keyman:') or SameText(token, '$keymanonly:') or SameText(token, '$keyman:') or
          SameText(token, '$kmfl:') or SameText(token, '$weaver:')
        then token := '*compile-target'
        else token := '*constant';
    end;
  end;

  for i := 0 to High(HelpArray) do
  begin
    if WideSameText(HelpArray[i].Word, token) then
    begin
      if HelpArray[i].Topic <> ''
        then token := LowerCase(HelpArray[i].Topic)
        else token := LowerCase(HelpArray[i].Word);
      if HelpArray[i].Guide
        then token := SHelpTopic_LanguageGuide_Prefix + token
        else token := SHelpTopic_LanguageReference_Prefix + token;
      Exit(True);
    end;
  end;

  Result := False;
end;

end.
