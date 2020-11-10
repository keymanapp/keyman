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
  System.Classes,
  System.SysUtils,
  System.Variants,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.StdCtrls,
  Winapi.Messages,
  Winapi.Windows,

  Keyman.Setup.System.InstallInfo;

type
  TfrmInstallOptions = class(TForm)
    chkStartWithWindows: TCheckBox;
    chkStartAfterInstall: TCheckBox;
    chkCheckForUpdates: TCheckBox;
    chkUpgradeKeyman7: TCheckBox;
    cmdOK: TButton;
    cmdCancel: TButton;
    chkAutomaticallyReportUsage: TCheckBox;
    sbTargets: TScrollBox;
    lblInstallOptions: TLabel;
    lblDefaultKeymanSettings: TLabel;
    lblSelectModulesToInstall: TLabel;
    lblAssociatedKeyboardLanguage: TLabel;
    lblInstallerVersion: TLabel;
    lblTitleLocation: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure cmdOKClick(Sender: TObject);
  private
    type TPackageControl = record
      Package: TInstallInfoPackage;
      Location: TInstallInfoPackageFileLocation;
      CheckBox: TCheckBox;
      ComboBoxLocation: TComboBox;
      ComboBoxLanguage: TComboBox;
      function IsValid: Boolean;
    end;
  private
    chkInstallKeyman: TCheckBox;
    cbKeymanLocation: TComboBox;
    FPackages: array of TPackageControl;
    function GetCanUpgradeKeyman7: Boolean;
    function GetCheckForUpdates: Boolean;
    function GetStartAfterInstall: Boolean;
    function GetStartWithWindows: Boolean;
    function GetUpgradeKeyman7: Boolean;
    procedure SetCanUpgradeKeyman7(const Value: Boolean);
    procedure SetCheckForUpdates(const Value: Boolean);
    procedure SetStartAfterInstall(const Value: Boolean);
    procedure SetStartWithWindows(const Value: Boolean);
    procedure SetUpgradeKeyman7(const Value: Boolean);
    function GetAutomaticallyReportUsage: Boolean;
    procedure SetAutomaticallyReportUsage(const Value: Boolean);
    procedure SetupDynamicOptions;
    procedure cbInstallKeyboardClick(Sender: TObject);
    procedure chkInstallKeyboardClick(Sender: TObject);
    procedure chkInstallKeymanClick(Sender: TObject);
    procedure EnableControls;
    procedure AddCheckboxPanel(const Text: string; AddLanguageCombo: Boolean;
      var pt: TPoint; var chk: TCheckBox; var cbLocation, cbLanguage: TComboBox);
    { Private declarations }
  public
    { Public declarations }
    property StartWithWindows: Boolean read GetStartWithWindows write SetStartWithWindows;
    property StartAfterInstall: Boolean read GetStartAfterInstall write SetStartAfterInstall;
    property CheckForUpdates: Boolean read GetCheckForUpdates write  SetCheckForUpdates;
    property UpgradeKeyman: Boolean read GetUpgradeKeyman7 write SetUpgradeKeyman7;
    property CanUpgradeKeyman: Boolean read GetCanUpgradeKeyman7 write SetCanUpgradeKeyman7;
    property AutomaticallyReportUsage: Boolean read GetAutomaticallyReportUsage write SetAutomaticallyReportUsage;
  end;

implementation

{$R *.dfm}

uses
  bootstrapmain,
  KeymanVersion,
  SetupStrings;

{ TfrmInstallOptions }

procedure TfrmInstallOptions.FormCreate(Sender: TObject);
var
  FAllowOptions: Boolean;
begin
  Caption := FInstallInfo.Text(ssOptionsTitle);
  chkStartWithWindows.Caption := FInstallInfo.Text(ssOptionsStartWithWindows);
  chkStartAfterInstall.Caption := FInstallInfo.Text(ssOptionsStartAfterInstall);
  chkCheckForUpdates.Caption := FInstallInfo.Text(ssOptionsCheckForUpdates);
  // Keyman 11 and later version keyboards will always automatically update
  chkUpgradeKeyman7.Caption := FInstallInfo.Text(ssOptionsUpgradeKeyboards);
  chkAutomaticallyReportUsage.Caption := FInstallInfo.Text(ssOptionsAutomaticallyReportUsage);
  cmdOK.Caption := FInstallInfo.Text(ssOkButton);
  cmdCancel.Caption := FInstallInfo.Text(ssCancelButton);
  lblInstallOptions.Caption := FInstallInfo.Text(ssOptionsTitleInstallOptions);
  lblDefaultKeymanSettings.Caption := FInstallInfo.Text(ssOptionsTitleDefaultKeymanSettings);
  lblSelectModulesToInstall.Caption := FInstallInfo.Text(ssOptionsTitleSelectModulesToInstall);
  lblAssociatedKeyboardLanguage.Caption := FInstallInfo.Text(ssOptionsTitleAssociatedKeyboardLanguage);
  lblTitleLocation.Caption := FInstallInfo.Text(ssOptionsTitleLocation);

  FAllowOptions := not FInstallInfo.IsInstalled and FInstallInfo.IsNewerAvailable;
  lblDefaultKeymanSettings.Visible := FAllowOptions;
  chkAutomaticallyReportUsage.Visible := FAllowOptions;
  chkCheckForUpdates.Visible := FAllowOptions;
  chkStartWithWindows.Visible := FAllowOptions;

  lblInstallerVersion.Caption := FInstallInfo.Text(ssInstallerVersion, [KeymanVersion.CKeymanVersionInfo.VersionWithTag]);

  SetupDynamicOptions;
end;

function TfrmInstallOptions.GetAutomaticallyReportUsage: Boolean;
begin
  Result := chkAutomaticallyReportUsage.Checked;
end;

function TfrmInstallOptions.GetCanUpgradeKeyman7: Boolean;
begin
  Result := chkUpgradeKeyman7.Enabled;
end;

function TfrmInstallOptions.GetCheckForUpdates: Boolean;
begin
  Result := chkCheckForUpdates.Checked;
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

procedure TfrmInstallOptions.SetAutomaticallyReportUsage(const Value: Boolean);
begin
  chkAutomaticallyReportUsage.Checked := Value;
end;

procedure TfrmInstallOptions.SetCanUpgradeKeyman7(const Value: Boolean);
begin
  chkUpgradeKeyman7.Enabled := Value;
end;

procedure TfrmInstallOptions.SetCheckForUpdates(const Value: Boolean);
begin
  chkCheckForUpdates.Checked := Value;
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

procedure TfrmInstallOptions.AddCheckboxPanel(const Text: string; AddLanguageCombo: Boolean; var pt: TPoint; var chk: TCheckBox; var cbLocation, cbLanguage: TComboBox);
var
  pan: TPanel;
begin
  pan := TPanel.Create(Self);
  chk := TCheckBox.Create(Self);
  cbLocation := TComboBox.Create(Self);
  cbLocation.Font := Font;

  pan.Left := pt.X;
  pan.Top := pt.Y;
  pan.Width := sbTargets.Width - GetSystemMetrics(SM_CXVSCROLL);
  pan.Height := cbLocation.Height;
  pan.BevelKind := bkTile;
  pan.BevelOuter := bvNone;
  pan.Caption := '';

  sbTargets.InsertControl(pan);

  chk.Left := 2;
  chk.Top := 2;
  chk.Width := sbTargets.ClientWidth div 3 + 1;
  chk.Caption := Text;
  pan.InsertControl(chk);

  if AddLanguageCombo then
  begin
    cbLanguage := TComboBox.Create(Self);
    cbLanguage.Left := pan.ClientWidth div 3 + 1;
    cbLanguage.Top := 0;
    cbLanguage.Width := pan.ClientWidth div 3;
    cbLanguage.Style := csDropDownList;
    pan.InsertControl(cbLanguage);
  end;

  cbLocation.Left := pan.ClientWidth - pan.ClientWidth div 3 - 1;
  cbLocation.Top := 0;
  cbLocation.Width := pan.ClientWidth div 3;
  cbLocation.Style := csDropDownList;
  pan.InsertControl(cbLocation);

  Inc(pt.Y, pan.Height - 2);
end;

procedure TfrmInstallOptions.SetupDynamicOptions;
var
  pack: TInstallInfoPackage;
  packLocation: TInstallInfoPackageFileLocation;
  chk: TCheckBox;
  pt: TPoint;
  cbLocation, cbLanguage: TComboBox;
  n: Integer;
  Text: string;
  selectedLang, lang: TInstallInfoPackageLanguage;
  location: TInstallInfoFileLocation;
begin
  pt.x := 0;
  pt.y := 0;

  if FInstallInfo.IsNewerAvailable then
  begin
    if FInstallInfo.IsInstalled
      then Text := FInstallInfo.Text(ssOptionsUpgradeKeyman)
      else Text := FInstallInfo.Text(ssOptionsInstallKeyman);

{
    if not FInstallInfo.IsInstalled then
      case FInstallInfo.BestMsi.LocationType of
        iilLocal:  Text := FInstallInfo.Text(ssOptionsInstallKeyman, [FInstallInfo.BestMsi.Version]);
        iilOnline: Text := FInstallInfo.Text(ssOptionsDownloadInstallKeyman, [FInstallInfo.BestMsi.Version, FormatFileSize(FInstallInfo.BestMsi.Size)]);
      end
    else
      case FInstallInfo.BestMsi.LocationType of
        iilLocal:  Text := FInstallInfo.Text(ssOptionsUpgradeKeyman, [FInstallInfo.BestMsi.Version]);
        iilOnline: Text := FInstallInfo.Text(ssOptionsDownloadUpgradeKeyman, [FInstallInfo.BestMsi.Version, FormatFileSize(FInstallInfo.BestMsi.Size)]);
      end;
}
  end
  else if FInstallInfo.IsInstalled then
  begin
    Text := FInstallInfo.Text(ssOptionsKeymanAlreadyInstalled, [FInstallInfo.InstalledVersion.Version]);
  end
  else
  begin
    // This is an error condition. We should not get here because the pre-installation checks should have forbidden it
    Assert(False, 'Offline, Keyman is not installed, and no msi is available');
  end;

  AddCheckboxPanel(Text, False, pt, chkInstallKeyman, cbKeymanLocation, cbLanguage);

  for location in FInstallInfo.MsiLocations do
  begin
    case location.LocationType of
      iilLocal: Text := FInstallInfo.Text(ssOptionsInstallKeymanVersion, [location.Version]);
      iilOnline: Text := FInstallInfo.Text(ssOptionsDownloadKeymanVersion, [location.Version, FormatFileSize(location.Size)]);
    end;
    cbKeymanLocation.Items.AddObject(Text, location);
  end;
  cbKeymanLocation.ItemIndex := cbKeymanLocation.Items.IndexOfObject(FInstallInfo.MsiInstallLocation);

  chkInstallKeyman.Checked := FInstallInfo.ShouldInstallKeyman;
  chkInstallKeyman.OnClick := chkInstallKeymanClick;
  chkInstallKeyman.Enabled := FInstallInfo.IsNewerAvailable and FInstallInfo.IsInstalled;
  cbKeymanLocation.Enabled := chkInstallKeyman.Enabled and (cbKeymanLocation.Items.Count > 1);

  lblTitleLocation.Left := cbKeymanLocation.Left + sbTargets.Left;

  n := 0;
  SetLength(FPackages, FInstallInfo.Packages.Count);
  for pack in FInstallInfo.Packages do
  begin
    packLocation := pack.InstallLocation;
    if not Assigned(packLocation) then Continue;

    Text := FInstallInfo.Text(ssOptionsInstallPackage, [packLocation.GetNameOrID(pack.ID)]);
    AddCheckboxPanel(Text, True, pt, chk, cbLocation, cbLanguage);
    chk.Checked := pack.ShouldInstall;
    chk.OnClick := chkInstallKeyboardClick;

    for location in pack.Locations DO
    begin
      case location.LocationType of
        iilLocal:  Text := FInstallInfo.Text(ssOptionsInstallPackageVersion, [location.Version]);
        iilOnline: Text := FInstallInfo.Text(ssOptionsDownloadPackageVersion, [location.Version, FormatFileSize(location.Size)]);
      end;
      cbLocation.Items.AddObject(Text, location);
    end;
    cbLocation.ItemIndex := cbLocation.Items.IndexOfObject(packLocation);
    cbLocation.OnClick := cbInstallKeyboardClick;

    cbLanguage.OnClick := cbInstallKeyboardClick;
    cbLanguage.Hint := FInstallInfo.Text(ssOptionsPackageLanguageAssociation, [packLocation.GetNameOrID(pack.ID)]);
    cbLanguage.ShowHint := True;

    selectedLang := nil;
    for lang in packLocation.Languages do
    begin
      cbLanguage.Items.AddObject(lang.Name + ' ('+lang.BCP47+')', lang);
      if SameText(pack.BCP47, lang.BCP47) then selectedLang := lang;
    end;

    if cbLanguage.Items.Count = 0 then
      cbLanguage.Items.AddObject(FInstallInfo.Text(ssOptionsDefaultLanguage), nil);

    cbLanguage.Sorted := True;
    cbLanguage.ItemIndex := cbLanguage.Items.IndexOfObject(selectedLang);
    if cbLanguage.ItemIndex < 0 then
      cbLanguage.ItemIndex := 0;

    FPackages[n].Package := pack;
    FPackages[n].Location := packLocation;
    FPackages[n].CheckBox := chk;
    FPackages[n].ComboBoxLanguage := cbLanguage;
    FPackages[n].ComboBoxLocation := cbLocation;
    Inc(n);

    lblAssociatedKeyboardLanguage.Left := cbLanguage.Left + sbTargets.Left;
  end;

  EnableControls;

  lblAssociatedKeyboardLanguage.Visible := FInstallInfo.Packages.Count > 0;

  // Special case: if there are no options to change, don't present them
  if (FInstallInfo.Packages.Count = 0) and not chkInstallKeyman.Enabled then
  begin
    sbTargets.Visible := False;
    lblSelectModulesToInstall.Visible := False;
    lblAssociatedKeyboardLanguage.Visible := False;
    lblTitleLocation.Visible := False;
  end;
end;

procedure TfrmInstallOptions.cmdOKClick(Sender: TObject);
var
  pack: TPackageControl;
begin
  FInstallInfo.ShouldInstallKeyman := chkInstallKeyman.Checked;
  FInstallInfo.MsiInstallLocation := cbKeymanLocation.Items.Objects[cbKeymanLocation.ItemIndex] as TInstallInfoFileLocation;

  for pack in FPackages do
    if pack.IsValid then
    begin
      pack.Package.ShouldInstall := pack.CheckBox.Checked;
      if (pack.ComboBoxLanguage.ItemIndex < 0) or
          not Assigned(pack.ComboBoxLanguage.Items.Objects[pack.ComboBoxLanguage.ItemIndex])
        then pack.Package.BCP47 := ''
        else pack.Package.BCP47 := (pack.ComboBoxLanguage.Items.Objects[pack.ComboBoxLanguage.ItemIndex] as TInstallInfoPackageLanguage).BCP47;
      pack.Package.InstallLocation := pack.ComboBoxLocation.Items.Objects[pack.ComboBoxLocation.ItemIndex] as TInstallInfoPackageFileLocation;
    end;

  ModalResult := mrOk;
end;

procedure TfrmInstallOptions.chkInstallKeymanClick(Sender: TObject);
begin
  EnableControls;
end;

procedure TfrmInstallOptions.chkInstallKeyboardClick(Sender: TObject);
begin
  EnableControls;
end;

procedure TfrmInstallOptions.cbInstallKeyboardClick(Sender: TObject);
begin
  EnableControls;
end;

procedure TfrmInstallOptions.EnableControls;
var
  i: Integer;
  e: Boolean;
begin
  chkInstallKeyman.Enabled := FInstallInfo.IsNewerAvailable and FInstallInfo.IsInstalled;

  e := chkInstallKeyman.Checked;
  cbKeymanLocation.Enabled := e and (cbKeymanLocation.Items.Count > 1);

  for i := 0 to High(FPackages) do
  begin
    if FPackages[i].IsValid then
    begin
      FPackages[i].ComboBoxLanguage.Enabled := FPackages[i].CheckBox.Checked and (FPackages[i].ComboBoxLanguage.Items.Count > 1);
      FPackages[i].ComboBoxLocation.Enabled := FPackages[i].CheckBox.Checked and (FPackages[i].ComboBoxLocation.Items.Count > 1);
      e := e or FPackages[i].CheckBox.Checked;
    end;
  end;
end;

{ TfrmInstallOptions.TPackageControl }

function TfrmInstallOptions.TPackageControl.IsValid: Boolean;
begin
  Result := Assigned(Package);
end;

end.
