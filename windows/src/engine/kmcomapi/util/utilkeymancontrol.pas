(*
  Name:             utilkeymancontrol
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    25 Oct 2016
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Aug 2006 - mcdurdin - Add HideVisualKeyboard and ShowVisualKeyboard
                    14 Sep 2006 - mcdurdin - Get path of kmcomapi.dll, not calling application
                    04 Dec 2006 - mcdurdin - Support running as elevated in Vista
                    16 May 2007 - mcdurdin - Return extra information if Keyman unable to start
                    30 May 2007 - mcdurdin - Refactor OS checks for newer OS versions
                    27 Mar 2008 - mcdurdin - Use the first active product for control when no product passed
                    12 Mar 2010 - mcdurdin - I2230 - Resolve crashes due to incorrect reference counting
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3331 - V9.0 - Remove koRunElevatedInVista option
                    08 Jun 2012 - mcdurdin - I3349 - V9.0 - Consolidate all process creation into TUtilExecute
                    25 Oct 2016 - mcdurdin - I5125 - Failure to start Keyman due to path errors
*)
unit utilkeymancontrol;

interface

uses Windows, custinterfaces, keymanapi_TLB;

type
  TKeymanController = class
  private
    FKeyman: TObject;
    procedure ControlKeyman(const code: string);
  public
    constructor Create(AKeyman: TObject);
    procedure StartKeyman;
    procedure StopKeyman;
    procedure ShowVisualKeyboard;
    procedure HideVisualKeyboard;
  end;

implementation

uses
  System.SysUtils,
  System.Variants,

  ErrorControlledRegistry,
  RegistryKeys,
  keymanerrorcodes,
  GetOsVersion,
  KeymanOptionNames,
  keymanpaths,
  utilexecute,
  keyman_implementation;

{ TKeymanController }

constructor TKeymanController.Create(AKeyman: TObject);
begin
  inherited Create;
  FKeyman := AKeyman;
end;

procedure TKeymanController.StartKeyman;
begin
  ControlKeyman('start');
end;

procedure TKeymanController.StopKeyman;
begin
  ControlKeyman('stop');
end;

procedure TKeymanController.ControlKeyman(const code: string);
var
  verb, path: string;
  hwnd: THandle;
begin
  path := TKeymanPaths.KeymanEngineInstallPath(TKeymanPaths.S_KeymanExe);

  if not FileExists(path) then
  begin
    (FKeyman as TKeyman).Errors.AddFmt(KMN_E_KeymanControl_CannotStartProduct, VarArrayOf([path, 'Keyman']), kesFatal);
    Exit;
  end;

  hwnd := GetActiveWindow;
  if hwnd = 0 then
    hwnd := GetDesktopWindow;

  if ((FKeyman as TKeyman).Options as IKeymanOptions).Items[koDebugging].Value = True then
  begin
    // We run Keyman elevated when Debugging mode is switched on. This is because starting a
    // trace requires certain permissions which a normal user doesn't have (refer StartTrace
    // documentation). This is also a good visual reminder to the user that Debugging is
    // active and as it is slightly annoying to be prompted to elevate, will over time encourage
    // them to switch it off again, which is good ;-)
    verb := 'runas';
  end
  else
    verb := 'open';

  if not TUtilExecute.Shell(hwnd, path, ExtractFileDir(path), '-kmc '+code, SW_SHOWNORMAL, verb) then  // I3349
    (FKeyman as TKeyman).Errors.AddFmt(KMN_E_KeymanControl_CannotStartProduct, VarArrayOf([path + ' (error='+IntToStr(GetLastError)+','+SysErrorMessage(GetLastError)+')', 'Keyman']), kesFatal);
end;

procedure TKeymanController.HideVisualKeyboard;
begin
  ControlKeyman('hidevisualkeyboard');
end;

procedure TKeymanController.ShowVisualKeyboard;
begin
  ControlKeyman('showvisualkeyboard');
end;

end.
