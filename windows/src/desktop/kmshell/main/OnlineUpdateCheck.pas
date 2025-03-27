(*
  Name:             OnlineUpdateCheck
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      4 Dec 2006

  Modified Date:    8 Jun 2012
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          04 Dec 2006 - mcdurdin - Support download progress
                    12 Dec 2006 - mcdurdin - Don't shutdown if update is cancelled
                    14 Dec 2006 - mcdurdin - Only test for patches, not downloads
                    04 Jan 2007 - mcdurdin - Add proxy support
                    15 Jan 2007 - mcdurdin - Show nice error message for online update check when offline
                    30 May 2007 - mcdurdin - I825 - Added proxy username and password
                    30 May 2007 - mcdurdin - I817 - Pass version information from external application
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    16 Jan 2009 - mcdurdin - I1730 - Check update of keyboards (refactor from global)
                    14 Jun 2009 - mcdurdin - I1704 - Activation through a firewall
                    26 Jul 2010 - mcdurdin - I2467 - 8.0 renumber
                    18 Oct 2010 - mcdurdin - I2513 - Online update for package fails to install when package installed by admin
                    21 Feb 2011 - mcdurdin - I2738 - Online update should be silent
                    21 Feb 2011 - mcdurdin - I2742 - No error message is given if downloading a file fails in online update
                    31 Mar 2011 - mcdurdin - I2855 - Keyman Developer online update crashes with Integer Overflow
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3349 - V9.0 - Consolidate all process creation into TUtilExecute
*)
unit OnlineUpdateCheck;  // I3306

interface

uses
  System.SysUtils,
  Vcl.Forms,

  UfrmDownloadProgress;
type
  EOnlineUpdateCheck = class(Exception);

  TOnlineUpdateCheckResult = (oucUnknown, oucShutDown, oucSuccess, oucNoUpdates, oucFailure, oucOffline);

  TOnlineUpdateCheckParamsPackage = record
    ID: string;
    NewID: string;
    Description: string;
    OldVersion, NewVersion: string;
    DownloadURL: string;
    SavePath: string;
    FileName: string;
    DownloadSize: Integer;
    Install: Boolean;
  end;

  TOnlineUpdateCheckParamsKeyman = record
    OldVersion, NewVersion: string;
    DownloadURL: string;
    SavePath: string;
    FileName: string;
    DownloadSize: Integer;
    Install: Boolean;
  end;

  TOnlineUpdateCheckParams = record
    Keyman: TOnlineUpdateCheckParamsKeyman;
    Packages: array of TOnlineUpdateCheckParamsPackage;
    Result: TOnlineUpdateCheckResult;
  end;

  TOnlineUpdateCheckDownloadParams = record
    Owner: TfrmDownloadProgress;
    TotalSize: Integer;
    TotalDownloads: Integer;
    StartPosition: Integer;
  end;

  TOnlineUpdateCheck = class
  private
    FOwner: TCustomForm;
    FSilent: Boolean;
    FForce: Boolean;
    FParams: TOnlineUpdateCheckParams;
    FErrorMessage: string;

    FShowErrors: Boolean;
    FCheckOnly: Boolean;
  public
    constructor Create(AOwner: TCustomForm; AForce, ASilent: Boolean; ACheckOnly: Boolean = False);
    destructor Destroy; override;
    property ShowErrors: Boolean read FShowErrors write FShowErrors;
  end;

  IOnlineUpdateSharedData = interface
    ['{7442A323-C1E3-404B-BEEA-5B24A52BBB0E}']
    function Params: TOnlineUpdateCheckParams;
  end;

  TOnlineUpdateSharedData = class(TInterfacedObject, IOnlineUpdateSharedData)
  private
    FParams: TOnlineUpdateCheckParams;
  public
    constructor Create(AParams: TOnlineUpdateCheckParams);
    function Params: TOnlineUpdateCheckParams;
  end;
implementation
uses
  Vcl.Dialogs,
  KLog;


const
  SPackageUpgradeFilename = 'upgrade_packages.inf';

{ TOnlineUpdateCheck }

constructor TOnlineUpdateCheck.Create(AOwner: TCustomForm; AForce, ASilent: Boolean; ACheckOnly: Boolean);
begin
  inherited Create;

  FOwner := AOwner;

  FShowErrors := True;
  FParams.Result := oucUnknown;

  FSilent := ASilent;
  FForce := AForce;
  FCheckOnly := ACheckOnly;

  KL.Log('TOnlineUpdateCheck.Create');
end;

destructor TOnlineUpdateCheck.Destroy;
begin
  if (FErrorMessage <> '') and not FSilent and FShowErrors then
    ShowMessage(FErrorMessage);

  KL.Log('TOnlineUpdateCheck.Destroy: FErrorMessage = '+FErrorMessage);
  KL.Log('TOnlineUpdateCheck.Destroy: FParams.Result = '+IntToStr(Ord(FParams.Result)));

  inherited Destroy;
end;

{ TOnlineUpdateSharedData }

constructor TOnlineUpdateSharedData.Create(AParams: TOnlineUpdateCheckParams);
begin
  inherited Create;
  FParams := AParams;
end;

function TOnlineUpdateSharedData.Params: TOnlineUpdateCheckParams;
begin
  Result := FParams;
end;

end.
