(*
  Name:             UfrmSelectWindowsLanguages
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      16 Jun 2008

  Modified Date:    24 Jul 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          16 Jun 2008 - mcdurdin - I1400 - Initial version
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    24 Jul 2015 - mcdurdin - I4796 - Refresh Keyman Developer look and feel for release
*)
unit UfrmSelectWindowsLanguages;  // I3306   // I4796

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UfrmTike, StdCtrls, WindowsLanguages;

type
  TfrmSelectWindowsLanguages = class(TTikeForm)
    lbLanguages: TListBox;
    cmdOK: TButton;
    cmdCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure lbLanguagesClick(Sender: TObject);
  private
    function GetLanguageCount: Integer;
    function GetLanguageNames(Index: Integer): WideString;
    function GetLanguages(Index: Integer): Integer;
    { Private declarations }
  protected
    function GetHelpTopic: string; override;
  public
    { Public declarations }
    procedure RemoveLanguage(ID: Integer);
    property LanguageCount: Integer read GetLanguageCount;
    property LanguageNames[Index: Integer]: WideString read GetLanguageNames;
    property Languages[Index: Integer]: Integer read GetLanguages;
  end;

implementation

uses
  Keyman.Developer.System.HelpTopics;

{$R *.dfm}

procedure TfrmSelectWindowsLanguages.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  cmdOK.Enabled := False;
  for i := 0 to High(CWindowsLanguages) do
    lbLanguages.Items.AddObject(CWindowsLanguages[i].Name, Pointer(CWindowsLanguages[i].ID));
end;

function TfrmSelectWindowsLanguages.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_SelectWindowsLanguages;
end;

function TfrmSelectWindowsLanguages.GetLanguageCount: Integer;
begin
  Result := lbLanguages.SelCount;
end;

function TfrmSelectWindowsLanguages.GetLanguageNames(Index: Integer): WideString;
var
  i, j: Integer;
begin
  j := 0;
  for i := 0 to lbLanguages.Items.Count - 1 do
    if lbLanguages.Selected[i] then
    begin
      if j = Index then
      begin
        Result := lbLanguages.Items[i];
        Exit;
      end;
      Inc(j);
    end;
  Result := '';
end;

function TfrmSelectWindowsLanguages.GetLanguages(Index: Integer): Integer;
var
  i, j: Integer;
begin
  j := 0;
  for i := 0 to lbLanguages.Items.Count - 1 do
    if lbLanguages.Selected[i] then
    begin
      if j = Index then
      begin
        Result := Integer(lbLanguages.Items.Objects[i]);
        Exit;
      end;
      Inc(j);
    end;
  Result := 0;
end;

procedure TfrmSelectWindowsLanguages.lbLanguagesClick(Sender: TObject);
begin
  cmdOK.Enabled := lbLanguages.SelCount > 0;
end;

procedure TfrmSelectWindowsLanguages.RemoveLanguage(ID: Integer);
var
  n: Integer;
begin
  n := lbLanguages.Items.IndexOfObject(Pointer(ID));
  if n >= 0 then
    lbLanguages.Items.Delete(n);
end;

end.
