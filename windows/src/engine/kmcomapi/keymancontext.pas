(*
  Name:             keymancontext
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    12 Mar 2010
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Add Control property
                    27 Mar 2008 - mcdurdin - Add Languages and Options access
                    12 Mar 2010 - mcdurdin - I2230 - Resolve crashes due to incorrect reference counting
*)
unit keymancontext;

interface

uses
  comobj,
  keymanapi_tlb,
  internalinterfaces,
  utilkeymancontrol;

type
  TKeymanContext = class
  private
    FKeyman: TObject;
    FController: TKeymanController;
    function GetErrors: IIntKeymanErrors;
    function GetKeyboards: IIntKeymanKeyboardsInstalled;
    function GetPackages: IIntKeymanPackagesInstalled;
    function GetSystemInfo: IIntKeymanSystemInfo;
    function GetControl: IIntKeymanControl;
    function GetOptions: IIntKeymanOptions;
    function GetLanguages: IIntKeymanLanguages;
  public
    constructor Create(AKeyman: TObject);
    destructor Destroy; override;
    property Errors: IIntKeymanErrors read GetErrors;
    property Keyboards: IIntKeymanKeyboardsInstalled read GetKeyboards;
    property Languages: IIntKeymanLanguages read GetLanguages;
    property Options: IIntKeymanOptions read GetOptions;
    property Packages: IIntKeymanPackagesInstalled read GetPackages;
    property SystemInfo: IIntKeymanSystemInfo read GetSystemInfo;
    property Control: IIntKeymanControl read GetControl;
    property Controller: TKeymanController read FController;
  end;

implementation

uses
  SysUtils, keyman_implementation;

{ TKeymanContext }

constructor TKeymanContext.Create(AKeyman: TObject);
begin
  inherited Create;
  FKeyman := AKeyman;
  FController := TKeymanController.Create(FKeyman);
end;

destructor TKeymanContext.Destroy;
begin
  FreeAndNil(FController);
  inherited Destroy;
end;

function TKeymanContext.GetControl: IIntKeymanControl;
begin
  Result := (FKeyman as TKeyman).Control;
end;

function TKeymanContext.GetErrors: IIntKeymanErrors;
begin
  Result := (FKeyman as TKeyman).Errors;
end;

function TKeymanContext.GetKeyboards: IIntKeymanKeyboardsInstalled;
begin
  Result := (FKeyman as TKeyman).Keyboards;
end;

function TKeymanContext.GetLanguages: IIntKeymanLanguages;
begin
  Result := (FKeyman as TKeyman).Languages;
end;

function TKeymanContext.GetOptions: IIntKeymanOptions;
begin
  Result := (FKeyman as TKeyman).Options;
end;

function TKeymanContext.GetPackages: IIntKeymanPackagesInstalled;
begin
  Result := (FKeyman as TKeyman).Packages;
end;

function TKeymanContext.GetSystemInfo: IIntKeymanSystemInfo;
begin
  Result := (FKeyman as TKeyman).SystemInfo;
end;

end.


