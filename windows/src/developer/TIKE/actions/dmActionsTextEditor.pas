(*
  Name:             dmActionsTextEditor
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      23 Aug 2006

  Modified Date:    24 Jul 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          23 Aug 2006 - mcdurdin - Initial version
                    30 Aug 2006 - mcdurdin - Add Reformat XML action
                    04 Dec 2006 - mcdurdin - Localize
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    22 Oct 2010 - mcdurdin - I2520 - Ctrl+Shift+U behaviour inconsistent
                    17 Dec 2010 - mcdurdin - I2595 - Remove GnuGetTExt
                    18 Mar 2011 - mcdurdin - I2520 - Improve Ctrl+Shift+U at start of line
                    18 Mar 2011 - mcdurdin - I2520 - Improve Ctrl+Shift+U for SMP
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    18 May 2012 - mcdurdin - I3323 - V9.0 - Change from Plus-MemoU to Plus-Memo
                    08 Jun 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
                    24 Jul 2015 - mcdurdin - I4797 - Convert to characters tool is inconsistent
*)
unit dmActionsTextEditor;  // I3306  // I3323

interface

uses
  System.Actions,
  System.Character,
  System.Classes,
  System.SysUtils,
  System.Types,
  Vcl.ActnList,
  Vcl.Menus,

  dmActionsMain;

type
  TmodActionsTextEditor = class(TDataModule)
    actionsTextEditor: TActionList;
    actTextEditor_ShowCharacter: TAction;
    actTextEditor_ConvertToCharacters: TAction;
    actTextEditor_ReformatXML: TAction;
    procedure actTextEditor_ConvertToCharactersExecute(Sender: TObject);
    procedure actTextEditor_ShowCharacterExecute(Sender: TObject);
    procedure actTextEditor_ShowCharacterUpdate(Sender: TObject);
    procedure actTextEditor_ConvertToCharactersUpdate(Sender: TObject);
    procedure actTextEditor_ReformatXMLUpdate(Sender: TObject);
    procedure actTextEditor_ReformatXMLExecute(Sender: TObject);
  end;

var
  modActionsTextEditor: TmodActionsTextEditor;

implementation

uses
  Vcl.Forms,

  KMDActionInterfaces,
  KMDActions,
  KeyboardParser,
  TextFileFormat,
  UframeTextEditor,
  UfrmMain,
  Unicode,
  utilstr,
  xmldoc;

{$R *.dfm}

procedure TmodActionsTextEditor.actTextEditor_ConvertToCharactersExecute(Sender: TObject);
var
  a: IKMDTextEditorActions;
  FRange: TRect;
  i, len: Integer;
  res, ws, FText, newtext, seltext: string;
  FError, FInQuotes, FToCodes: Boolean;
begin
  a := KMDActions.GetTextEditorController(Screen.ActiveControl);

  FText := a.Text;
  FRange := a.SelectedRange;

  with TStringList.Create do
  try
    Text := FText;
    if (FRange.Left = FRange.Right) and (FRange.Bottom = FRange.Top) then
    begin
      // No selection
      Inc(FRange.Left); // 1-offset instead of 0-offset
      seltext := GetTokenFromCaret(Strings[FRange.Top], FRange.Left, len);
      FRange.Right := FRange.Left + len;
    end
    else if FRange.Top = FRange.Bottom then
    begin
      // Single line selection
      seltext := Copy(Strings[FRange.Top], FRange.Left + 1, FRange.Right - FRange.Left);

      // Adjust selection to remove whitespace
      newtext := seltext.TrimLeft;
      Inc(FRange.Left, seltext.Length - newtext.Length);
      seltext := newtext.TrimRight;
      Dec(FRange.Right, newtext.Length - seltext.Length);
     end
    else
      // Cannot convert multi-line selections at this time
      Exit;
  finally
    Free;
  end;

  if seltext = '' then
    Exit;

  ws := ExtStringToString(seltext, FError);
  if FError then
    Exit;

  FToCodes := (Pos('"', seltext) > 0) or (Pos('''', seltext) > 0);

  if FToCodes then
    res := StringToExtString(ws, True)
  else
  begin
    res := '';
    FInQuotes := False;
    for i := 1 to Length(ws) do
      if ws[i] = '''' then
      begin
        if FInQuotes then
          res := res + ''' ';
        FInQuotes := False;
        res := res + '"''"';
      end
      else
      begin
        if not FInQuotes then
          res := res + ' ''';
        FInQuotes := True;
        res := res + ws[i];
      end;
    if FInQuotes then
      res := res + '''';
    res := Trim(res);
  end;

  a.ReplaceSelection(FRange, res);
end;

procedure TmodActionsTextEditor.actTextEditor_ConvertToCharactersUpdate(Sender: TObject);
begin
  actTextEditor_ConvertToCharacters.Enabled := KMDActions.IsTextEditor(Screen.ActiveControl);
end;

procedure TmodActionsTextEditor.actTextEditor_ReformatXMLExecute(Sender: TObject);
var
  a: IKMDTextEditorActions;
begin
  a := KMDActions.GetTextEditorController(Screen.ActiveControl);
  a.Text := FormatXMLData(a.Text);
end;

procedure TmodActionsTextEditor.actTextEditor_ReformatXMLUpdate(
  Sender: TObject);
var
  a: IKMDTextEditorActions;
begin
  a := KMDActions.GetTextEditorController(Screen.ActiveControl);
  actTextEditor_ReformatXML.Enabled := Assigned(a) and (a.EditorFormat = efXML);
end;

procedure TmodActionsTextEditor.actTextEditor_ShowCharacterExecute(Sender: TObject);
begin
  if KMDActions.IsTextEditor(Screen.ActiveControl) then
  begin
    //TODO
  end;
end;

procedure TmodActionsTextEditor.actTextEditor_ShowCharacterUpdate(Sender: TObject);
begin
  actTextEditor_ConvertToCharacters.Enabled := KMDActions.IsTextEditor(Screen.ActiveControl);
end;

end.
