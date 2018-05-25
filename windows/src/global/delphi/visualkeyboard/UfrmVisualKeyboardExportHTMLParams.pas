(*
  Name:             UfrmVisualKeyboardExportHTMLParams
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      23 Aug 2006

  Modified Date:    3 Aug 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          23 Aug 2006 - mcdurdin - Initial refactor for new visual keyboard
                    29 Sep 2008 - mcdurdin - I1658 - Reworked for graphics options
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    03 Aug 2015 - mcdurdin - I4822 - Form sizes are incorrect with new theming
*)
unit UfrmVisualKeyboardExportHTMLParams;  // I3306

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, 
  StdCtrls, UfrmTike;

type
  TfrmVisualKeyboardExportHTMLParams = class(TTIKEForm)   // I4822
    cmdOK: TButton;
    cmdCancel: TButton;
    gbOutputType: TGroupBox;
    Label1: TLabel;
    lblFileName: TLabel;
    Label5: TLabel;
    rbFolders: TRadioButton;
    rbNoFolders: TRadioButton;
    chkGraphical: TCheckBox;
    Label2: TLabel;
    procedure cmdOKClick(Sender: TObject);
  private
    procedure SetFileName(const Value: string);
    function GetFolders: Boolean;
    function GetGraphical: Boolean;
  protected
    function GetHelpTopic: string; override;
  public
    property FileName: string write SetFileName;
    property Folders: Boolean read GetFolders;
    property Graphical: Boolean read GetGraphical;
  end;

implementation

{$R *.DFM}

uses
  Keyman.Developer.System.HelpTopics,

  messageidentifiers;

{ TfrmVisualKeyboardExportHTMLParams }

procedure TfrmVisualKeyboardExportHTMLParams.SetFileName(const Value: string);
begin
  lblFileName.Caption := ChangeFileExt(ExtractFileName(Value), '')+'_files';
end;

function TfrmVisualKeyboardExportHTMLParams.GetFolders: Boolean;
begin
  Result := rbFolders.Checked;
end;

function TfrmVisualKeyboardExportHTMLParams.GetGraphical: Boolean;
begin
  Result := chkGraphical.Checked;
end;

function TfrmVisualKeyboardExportHTMLParams.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_VisualKeyboardExportHtmlParams;
end;

procedure TfrmVisualKeyboardExportHTMLParams.cmdOKClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

end.

