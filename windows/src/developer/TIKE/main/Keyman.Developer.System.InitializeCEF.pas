unit Keyman.Developer.System.InitializeCEF;

interface

uses
  uCEFApplication;

//
// We wrap the TCefApplication so we can configure it
// outside the .dpr source and keep the .dpr source
// cleaner
//
type
  TInitializeCEF = class
  public
    constructor Create;
    destructor Destroy; override;
    function Start: Boolean;
  end;

var
  FInitializeCEF: TInitializeCEF = nil;

implementation

uses
  Winapi.ShlObj,

  KeymanDeveloperUtils,
  RedistFiles,
  RegistryKeys,
  UTikeDebugMode;

{ TInitializeCEF }

constructor TInitializeCEF.Create;
begin
  // You *MUST* call GlobalCEFApp.StartMainProcess in a if..then clause
  // with the Application initialization inside the begin..end.
  // Read this https://www.briskbard.com/index.php?lang=en&pageid=cef
  GlobalCEFApp := TCefApplication.Create;

  // We run in debug mode when TIKE debug mode flag is set, so that we can
  // debug the CEF interactions more easily
  GlobalCEFApp.SingleProcess        := TikeDebugMode;

  // In case you want to use custom directories for the CEF3 binaries, cache, cookies and user data.
  // If you don't set a cache directory the browser will use in-memory cache.
  GlobalCEFApp.FrameworkDirPath     := GetCEFPath;
  GlobalCEFApp.ResourcesDirPath     := GlobalCEFApp.FrameworkDirPath;
  GlobalCEFApp.LocalesDirPath       := GlobalCEFApp.FrameworkDirPath + '\locales';
  GlobalCEFApp.EnableGPU            := True;      // Enable hardware acceleration
  GlobalCEFApp.cache                := GetFolderPath(CSIDL_APPDATA) + SFolderKeymanDeveloper + '\browser\cache';  //TODO: refactor into KeymanPaths.pas
  GlobalCEFApp.cookies              := GetFolderPath(CSIDL_APPDATA) + SFolderKeymanDeveloper + '\browser\cookies';
  GlobalCEFApp.UserDataPath         := GetFolderPath(CSIDL_APPDATA) + SFolderKeymanDeveloper + '\browser\userdata';
end;

destructor TInitializeCEF.Destroy;
begin
  GlobalCEFApp.Free;
  GlobalCEFApp := nil;
  inherited Destroy;
end;

function TInitializeCEF.Start: Boolean;
begin
  Result := GlobalCEFApp.StartMainProcess;
end;

end.
