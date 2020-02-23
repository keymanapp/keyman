(*
  Name:             KMShellHints
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      27 Mar 2008

  Modified Date:    18 May 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          27 Mar 2008 - mcdurdin - I1256 - Initial version (hint system)
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
*)
unit KMShellHints;  // I3306

interface

uses
  Classes,
  Consts,
  Controls,
  Dialogs,
  Forms,
  Graphics,
  HintConsts,
  keymanapi_TLB,
  Math,
  Messages,
  WideStrUtils,
  SysUtils,
  Windows;

function ShowKMShellHintQuery(AHint: TKeymanHint; AButtons: TMsgDlgButtons; ADefaultResult: TModalResult): TModalResult; overload;
function ShowKMShellHintQuery(AHint: string): TModalResult; overload;

implementation

uses
  Hints,
  UfrmHint,
  XMLRenderer;

function ShowKMShellHintQuery(AHint: string): TModalResult; overload;
var
  h: TKeymanHint;
begin
  h := GetHintFromName(AHint);
  if h = KH_NULL then
    Exit(mrOk);

  if KeymanHintData[h].IsQuestion
    then Result := ShowKMShellHintQuery(h, mbOKCancel, mrOk)
    else Result := ShowKMShellHintQuery(h, [mbOk], mrOk);
end;

function ShowKMShellHintQuery(AHint: TKeymanHint; AButtons: TMsgDlgButtons; ADefaultResult: TModalResult): TModalResult; overload;
begin
  Result := ADefaultResult;
  if IsHintEnabled(AHint) then
  begin
    if FileExists(GetXMLTemplatePath('hint.xsl')+'hint.xsl') then
    begin
      with TfrmHint.Create(Screen.ActiveForm) do
      try
        Hint := AHint;
        Buttons := AButtons;
        DefaultResult := ADefaultResult;
        FormStyle := fsStayOnTop;
        Result := ShowModal;
      finally
        Free;
      end;
    end;
  end;
end;

end.
