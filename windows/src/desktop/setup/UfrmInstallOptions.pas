(*
  Name:             UfrmInstallOptions
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      30 Dec 2010

  Modified Date:    30 Dec 2010
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          30 Dec 2010 - mcdurdin - I2562 - Install options now a single dialog (new unit)
*)
unit UfrmInstallOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmInstallOptions = class(TForm)
    chkStartWithWindows: TCheckBox;
    chkStartAfterInstall: TCheckBox;
    chkCheckForUpdates: TCheckBox;
    chkCheckForUpdatesInstall: TCheckBox;
    chkUpgradeKeyman7: TCheckBox;
    cmdOK: TButton;
    cmdCancel: TButton;
    procedure FormCreate(Sender: TObject);
  private
    function GetCanUpgradeKeyman7: Boolean;
    function GetCheckForUpdates: Boolean;
    function GetCheckForUpdatesInstall: Boolean;
    function GetStartAfterInstall: Boolean;
    function GetStartWithWindows: Boolean;
    function GetUpgradeKeyman7: Boolean;
    procedure SetCanUpgradeKeyman7(const Value: Boolean);
    procedure SetCheckForUpdates(const Value: Boolean);
    procedure SetCheckForUpdatesInstall(const Value: Boolean);
    procedure SetStartAfterInstall(const Value: Boolean);
    procedure SetStartWithWindows(const Value: Boolean);
    procedure SetUpgradeKeyman7(const Value: Boolean);
    { Private declarations }
  public
    { Public declarations }
    property StartWithWindows: Boolean read GetStartWithWindows write SetStartWithWindows;
    property StartAfterInstall: Boolean read GetStartAfterInstall write SetStartAfterInstall;
    property CheckForUpdates: Boolean read GetCheckForUpdates write  SetCheckForUpdates;
    property CheckForUpdatesInstall: Boolean read GetCheckForUpdatesInstall write SetCheckForUpdatesInstall;
    property UpgradeKeyman: Boolean read GetUpgradeKeyman7 write SetUpgradeKeyman7;
    property CanUpgradeKeyman: Boolean read GetCanUpgradeKeyman7 write SetCanUpgradeKeyman7;
  end;

implementation

{$R *.dfm}

uses
  bootstrapmain,
  SetupStrings;

{ TfrmInstallOptions }

procedure TfrmInstallOptions.FormCreate(Sender: TObject);
begin
  Caption := FInstallInfo.Text(ssOptionsTitle);
  chkStartWithWindows.Caption := FInstallInfo.Text(ssOptionsStartWithWindows);
  chkStartAfterInstall.Caption := FInstallInfo.Text(ssOptionsStartAfterInstall);
  chkCheckForUpdates.Caption := FInstallInfo.Text(ssOptionsCheckForUpdates);
  chkCheckForUpdatesInstall.Caption := FInstallInfo.Text(ssOptionsCheckForUpdatesBeforeInstall);
  // Keyman 11 and later version keyboards will always automatically update
  chkUpgradeKeyman7.Caption := FInstallInfo.Text(ssOptionsUpgradeKeyboards);
end;

function TfrmInstallOptions.GetCanUpgradeKeyman7: Boolean;
begin
  Result := chkUpgradeKeyman7.Enabled;
end;

function TfrmInstallOptions.GetCheckForUpdates: Boolean;
begin
  Result := chkCheckForUpdates.Checked;
end;

function TfrmInstallOptions.GetCheckForUpdatesInstall: Boolean;
begin
  Result := chkCheckForUpdatesInstall.Checked;
end;

function TfrmInstallOptions.GetStartAfterInstall: Boolean;
begin
  Result := chkStartAfterInstall.Checked;
end;

function TfrmInstallOptions.GetStartWithWindows: Boolean;
begin
  Result := chkStartWithWindows.Checked;
end;

function TfrmInstallOptions.GetUpgradeKeyman7: Boolean;
begin
  Result := chkUpgradeKeyman7.Checked;
end;

procedure TfrmInstallOptions.SetCanUpgradeKeyman7(const Value: Boolean);
begin
  chkUpgradeKeyman7.Enabled := Value;
end;

procedure TfrmInstallOptions.SetCheckForUpdates(const Value: Boolean);
begin
  chkCheckForUpdates.Checked := Value;
end;

procedure TfrmInstallOptions.SetCheckForUpdatesInstall(const Value: Boolean);
begin
  chkCheckForUpdatesInstall.Checked := Value;
end;

procedure TfrmInstallOptions.SetStartAfterInstall(const Value: Boolean);
begin
  chkStartAfterInstall.Checked := Value;
end;

procedure TfrmInstallOptions.SetStartWithWindows(const Value: Boolean);
begin
  chkStartWithWindows.Checked := Value;
end;

procedure TfrmInstallOptions.SetUpgradeKeyman7(const Value: Boolean);
begin
  chkUpgradeKeyman7.Checked := Value;
end;

end.
