(*
  Name:             si_base
  Copyright:        Copyright (C) SIL International.
  Documentation:    km4.1
  Description:      Base classes for system info functions
  Create Date:      13 May 2005

  Modified Date:    3 Oct 2011
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          13 May 2005 - mcdurdin - Integrated into kmshell from tsysinfo
                    09 Jun 2005 - mcdurdin - Use MSXML_TLB not MSXML2_TLB
                    01 Aug 2006 - mcdurdin - Initial version for Keyman 7
                    23 Apr 2009 - mcdurdin - I1941 - Collect minidump files, diagnostic log files and zip them with the diagnostic report
                    19 Jul 2011 - mcdurdin - I3000 - Tweak display of diagnostics using .xslt files
                    03 Oct 2011 - mcdurdin - I2919 - Unicode support, xslts
*)
unit si_base;

interface

uses
  Windows, SysUtils, Classes, msxml, Contnrs;

type
  TSI_Base = class;
  TSIList = class;

  TSIClasses = class(TClassList);

  TSIList = class(TObjectList)
  private
    FFiles: TStrings;
    function GetItem(Index: Integer): TSI_Base;
    procedure SetItem(Index: Integer; const Value: TSI_Base);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Collect;
    procedure Save(const AFileName: string);
    property Files: TStrings read FFiles;
    property Items[Index: Integer]: TSI_Base read GetItem write SetItem; default;
  end;

  TSI_Base = class
  private
    FXMLData: WideString;
    //class function FrameClass: TSIFrameClass;
  protected
    rootnode: IXMLDOMNode;
    doc: IXMLDOMDocument;
    class procedure Register;
    function DoCollect: Boolean; virtual; abstract;
  public
    function Collect: Boolean;
    property XMLData: WideString read FXMLData;
  end;

  TSI_BaseClass = class of TSI_Base;

function SIClasses: TSIClasses;

implementation

uses System.Win.ComObj, sysinfo_util;

var
  FSIClasses: TSIClasses = nil;

{ TSI_Base }

function TSI_Base.Collect: Boolean;
var
  node: IXMLDOMNode;
begin
  doc := CreateOleObject('MSXML.DOMDocument') as IXMLDOMDocument;
  rootnode := doc.createNode(1, 'KeymanDiagnosticRecord', '');
  doc.appendChild(rootnode);
  try
    try
      Result := DoCollect;
    except
      on E:Exception do
      begin
        node := doc.createNode(1, 'Exception', '');
        node.text := XMLTextEncode(E.ClassName+': '+E.Message);
        rootnode.appendChild(node);
        Result := False;
      end;
    end;
  finally
    FXMLData := doc.xml;
    rootnode := nil;
    doc := nil;
  end;
end;

class procedure TSI_Base.Register;
begin
  if not Assigned(FSIClasses) then
    FSIClasses := TSIClasses.Create;
  FSIClasses.Add(Self);
end;

function SIClasses: TSIClasses;
begin
  if not Assigned(FSIClasses) then
    FSIClasses := TSIClasses.Create;
  Result := FSIClasses;
end;

{ TSIList }

procedure TSIList.Collect;
var
  si: TSI_Base;
  i: Integer;
begin
  for i := 0 to SIClasses.Count - 1 do
  begin
    si := TSI_BaseClass(SIClasses[i]).Create;
    Add(si);
    si.Collect;
  end;
end;

procedure TSIList.Save(const AFileName: string);
var
  i: Integer;
  s: WideString;
  t: AnsiString;
begin
  s := '<?xml version="1.0" encoding="utf-8" ?>'#13#10;  // I2919
  s := s + '<TavultesoftSystemInformation>';
  for i := 0 to Count - 1 do
    s := s + '<'+Items[i].ClassName+'>'+Items[i].FXMLData+'</'+Items[i].ClassName+'>'#13#10;
  s := s + '</TavultesoftSystemInformation>';

  with TFileStream.Create(AFileName, fmCreate) do
  try
    t := UTF8Encode(s);  // I2919
    Write(PAnsiChar(t)^, Length(t));
  finally
    Free;
  end;
end;

function TSIList.GetItem(Index: Integer): TSI_Base;
begin
  Result := inherited GetItem(Index) as TSI_Base;
end;

procedure TSIList.SetItem(Index: Integer; const Value: TSI_Base);
begin
  inherited SetItem(Index, Value);
end;

constructor TSIList.Create;
begin
  inherited Create;
  FFiles := TStringList.Create;
end;

destructor TSIList.Destroy;
begin
  FFiles.Free;
  inherited Destroy;
end;

initialization
finalization
  FreeAndNil(FSIClasses);
end.
