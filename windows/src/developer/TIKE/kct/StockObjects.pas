(*
  Name:             StockObjects
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    19 Nov 2007
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
                    04 Dec 2006 - mcdurdin - Add public CustStorage property
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
*)
unit StockObjects;

interface

uses
  Windows, Messages, SysUtils, Classes, contnrs, CustomisationStorage, StockMessages;

type
  EStockObject = class(Exception);
  
  TStockObjectManager = class;

  TStockObjectManager = class
  private
    FKCTFileName: string;
    FMessages: TStockMessages;
    FCustStorage: TCustomisationStorage;
  public
    constructor Create(AKCTFileName: string);
    destructor Destroy; override;
    function IndexOfFileName(FileName: string): Integer;
    property CustStorage: TCustomisationStorage read FCustStorage;
    property Messages: TStockMessages read FMessages;
  end;

implementation

{ TStockObjectManager }

constructor TStockObjectManager.Create(AKCTFileName: string);
var
  FMessagesCustFile: TCustFile;
begin
  inherited Create;
  FKCTFileName := AKCTFileName;
  if not FileExists(FKCTFileName) then
    raise EStockObject.Create('The branding stock object file '+FKCTFileName+' does not exist.');
  FCustStorage := TCustomisationStorage.Create(FKCTFileName);
  FMessagesCustFile := FCustStorage.GetFileOfType(ftMessages);
  if not Assigned(FMessagesCustFile) then
    raise EStockObject.Create('Message Identifiers missing from stock objects');
  FMessagesCustFile.Stream.Position := 0;
  FMessages := TStockMessages.Create(FMessagesCustFile.Stream);
end;

destructor TStockObjectManager.Destroy;
begin
  FreeAndNil(FMessages);
  FreeAndNil(FCustStorage);
  inherited Destroy;
end;

function TStockObjectManager.IndexOfFileName(FileName: string): Integer;
begin
  Result := FCustStorage.CustFiles.IndexOfFileName(FileName);
end;

end.
