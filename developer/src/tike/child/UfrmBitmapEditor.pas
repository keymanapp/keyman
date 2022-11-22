(*
  Name:             UfrmBitmapEditor
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    27 Feb 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Rework as tabchild
                    02 Aug 2006 - mcdurdin - Rework menus as sp-TBX
                    23 Aug 2006 - mcdurdin - Clean up menus
                    14 Sep 2006 - mcdurdin - Strip out old bitmap editor options and use a frame instead
                    04 Dec 2006 - mcdurdin - Add GetDefaultExt call
                    27 Feb 2014 - mcdurdin - I4081 - V9.0 - Trace compile errors in subfiles in Keyman Developer
*)
unit UfrmBitmapEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  UfrmMDIChild, ExtCtrls, PaintPanel, StdCtrls, Buttons, Menus, ImgList,
  MenuImgList, UfrmMDIEditor, UframeBitmapEditor, KMDActionInterfaces;

type
  TfrmBitmapEditor = class(TfrmTikeEditor)
    frame: TframeBitmapEditor;
    procedure FormCreate(Sender: TObject);
  private
    procedure FrameModified(Sender: TObject);
  protected
    function GetHelpTopic: string; override;
    function DoOpenFile: Boolean; override;
    function DoSaveFile: Boolean; override;
    function GetFileNameFilter: string; override;
    function GetDefaultExt: string; override;

  public
    procedure FindError(const Filename: string; s: string; line: Integer); override;   // I4081
  end;

implementation

uses
  Clipbrd,

  Keyman.Developer.System.HelpTopics,

  UfrmMain,
  UfrmMessages;

{$R *.DFM}

procedure TfrmBitmapEditor.FormCreate(Sender: TObject);
begin
  inherited;


  frame.OnModifiedChanged := FrameModified;
  //FModified := True;
  frame.Modified := False;
end;

function TfrmBitmapEditor.DoSaveFile: Boolean;
begin
  frame.SaveToFile(FileName);
  Result := True;
end;

function TfrmBitmapEditor.GetDefaultExt: string;
begin
  Result := 'bmp';
end;

function TfrmBitmapEditor.GetFileNameFilter: string;
begin
  Result := 'Bitmap files (*.bmp)|*.bmp|All files (*.*)|*.*';
end;

function TfrmBitmapEditor.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_BitmapEditor;
end;

function TfrmBitmapEditor.DoOpenFile: Boolean;
begin
  RefreshOptions;
  if FileExists(FileName) then
    frame.LoadFromFile(FileName);
  Result := True;
end;

procedure TfrmBitmapEditor.FrameModified(Sender: TObject);
begin
  Modified := frame.Modified;
end;

procedure TfrmBitmapEditor.FindError(const Filename: string; s: string; line: Integer);   // I4081
begin
  //
end;

end.

