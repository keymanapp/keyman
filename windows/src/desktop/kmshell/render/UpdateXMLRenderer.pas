(*
  Name:             UpdateXMLRenderer
  Copyright:        Copyright (C) SIL International.
*)
unit UpdateXMLRenderer;

interface

uses
  XMLRenderer,
  Windows;

type
  TUpdateXMLRenderer = class(TXMLRenderer)
  protected
    function XMLData: WideString; override;
  end;

implementation

uses
  StrUtils,
  SysUtils,
  VersionInfo,
  kmint,
  KeymanVersion,
  keymanapi_TLB,
  Keyman.System.LocaleStrings,
  Keyman.System.UpdateCheckResponse,
  Keyman.System.UpdateCheckStorage,
  MessageIdentifierConsts,
  MessageIdentifiers,
  OnlineUpdateCheck,
  utilxml;

{ TUpdateXMLRenderer }

function TUpdateXMLRenderer.XMLData: WideString;
var
  xml: string;
  ucr: TUpdateCheckResponse;
  ouc: TOnlineUpdateCheck;
  params: TOnlineUpdateCheckParams;
  i: Integer;
begin
  xml := '';

  if TUpdateCheckStorage.LoadUpdateCacheData(ucr) then
  begin
    ouc := TOnlineUpdateCheck.Create(nil, False, True);
    params := ouc.ResponseToParams(ucr);

    if (Params.Keyman.DownloadURL <> '') then
    begin
      xml := xml +
        '<Update>'+
          '<index>0</index>'+
          IfThen(not kmcom.SystemInfo.IsAdministrator, '<RequiresAdmin />')+
          '<Keyman>'+
            '<Text>'+xmlencode(TLocaleStrings.MsgFromIdFormat(kmcom, SKUpdate_KeymanText, [Params.Keyman.NewVersion]))+'</Text>'+
            '<NewVersion>'+xmlencode(Params.Keyman.NewVersion)+'</NewVersion>'+
            '<OldVersion>'+xmlencode(Params.Keyman.OldVersion)+'</OldVersion>'+
            '<DownloadSize>'+xmlencode(Format('%d', [Params.Keyman.DownloadSize div 1024]))+'KB</DownloadSize>'+
            '<DownloadURL>'+xmlencode(Params.Keyman.DownloadURL)+'</DownloadURL>'+
          '</Keyman>'+
        '</Update>';
    end;

    for i := 0 to High(Params.Packages) do
    begin
      xml := xml +
        '<Update>'+
          '<index>'+IntToStr(i+1)+'</index>'+
          IfThen(not kmcom.SystemInfo.IsAdministrator, '<RequiresAdmin />')+
          '<Package>'+
            '<Text>'+xmlencode(TLocaleStrings.MsgFromIdFormat(kmcom, SKUpdate_PackageText,
              [Params.Packages[i].Description, Params.Packages[i].NewVersion]))+'</Text>'+
            '<NewVersion>'+xmlencode(Params.Packages[i].NewVersion)+'</NewVersion>'+
            '<OldVersion>'+xmlencode(Params.Packages[i].OldVersion)+'</OldVersion>'+
            '<DownloadSize>'+xmlencode(Format('%d', [Params.Packages[i].DownloadSize div 1024]))+'KB</DownloadSize>'+
            '<DownloadURL>'+xmlencode(Params.Packages[i].DownloadURL)+'</DownloadURL>'+
          '</Package>'+
        '</Update>';
    end;
  end;

  Result := '<Updates>'+xml+'</Updates>';
end;

end.

