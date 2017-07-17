(*
  Name:             UfrmOSKEditor
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      4 Dec 2006

  Modified Date:    27 Feb 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          04 Dec 2006 - mcdurdin - Initial version
                    27 Feb 2014 - mcdurdin - I4081 - V9.0 - Trace compile errors in subfiles in Keyman Developer
*)
unit UfrmOSKEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UfrmMDIEditor, UframeOnScreenKeyboardEditor;

type
  TfrmOSKEditor = class(TfrmTikeEditor)
    procedure FormCreate(Sender: TObject);
  private
    frameOSK: TframeOnScreenKeyboardEditor;
    procedure OSKModified(Sender: TObject);
  protected
    function GetFileNameFilter: string; override;
    function GetDefaultExt: string; override;

//    procedure ModifiedChanged;

    function DoSaveFile: Boolean; override;
    function DoOpenFile: Boolean; override;
  public
    procedure FindError(const Filename: string; s: string); override;   // I4081
  end;

implementation

{$R *.dfm}

{ TfrmOSKEditor }

function TfrmOSKEditor.DoOpenFile: Boolean;
begin
  frameOSK.FileName := FileName;
  frameOSK.Load;
  Result := True;
end;

function TfrmOSKEditor.DoSaveFile: Boolean;
begin
  frameOSK.FileName := FileName;
  frameOSK.Save;
  Result := True;
end;

procedure TfrmOSKEditor.FindError(const Filename: string; s: string);   // I4081
begin
end;

procedure TfrmOSKEditor.FormCreate(Sender: TObject);
begin
  inherited;
  frameOSK := TframeOnScreenKeyboardEditor.Create(Self);
  frameOSK.Parent := Self;
  frameOSK.Align := alClient;
  frameOSK.Visible := True;
  frameOSK.OnModified := OSKModified;
end;

procedure TfrmOSKEditor.OSKModified(Sender: TObject);
begin
  Modified := frameOSK.VKModified;
end;

function TfrmOSKEditor.GetDefaultExt: string;
begin
  Result := '.kvk';
end;

function TfrmOSKEditor.GetFileNameFilter: string;
begin
  Result := 'On Screen Keyboard Files (*.kvk)|*.kvk';
end;

end.
