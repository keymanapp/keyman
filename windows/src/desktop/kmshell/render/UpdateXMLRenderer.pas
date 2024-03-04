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
  utilxml;

{ TUpdateXMLRenderer }

function TUpdateXMLRenderer.XMLData: WideString;
var
  xml: string;
  ucr: TUpdateCheckResponse;
  i, n : Integer;
  pkg: IKeymanPackage;
begin
  xml := '';

  if TUpdateCheckStorage.LoadUpdateCacheData(ucr) then
  begin
    if (ucr.InstallURL <> '') then
    begin
      xml := xml +
        '<Update>'+
          '<index>0</index>'+
          IfThen(not kmcom.SystemInfo.IsAdministrator, '<RequiresAdmin />')+
          '<Keyman>'+
            '<Text>'+xmlencode(TLocaleStrings.MsgFromIdFormat(kmcom, SKUpdate_KeymanText, [ucr.NewVersion]))+'</Text>'+
            '<NewVersion>'+xmlencode(ucr.NewVersion)+'</NewVersion>'+
            '<OldVersion>'+xmlencode(ucr.CurrentVersion)+'</OldVersion>'+
            '<DownloadSize>'+xmlencode(Format('%d', [ucr.InstallSize div 1024]))+'KB</DownloadSize>'+
            '<DownloadURL>'+xmlencode(ucr.InstallURL)+'</DownloadURL>'+
          '</Keyman>'+
        '</Update>';
    end;

    for i := 0 to High(ucr.Packages) do
    begin
      n := kmcom.Packages.IndexOf(ucr.Packages[i].ID);
      if n >= 0 then
      pkg := kmcom.Packages[n];
      begin
        xml := xml +
          '<Update>'+
            '<index>'+IntToStr(i+1)+'</index>'+
            IfThen(not kmcom.SystemInfo.IsAdministrator, '<RequiresAdmin />')+
            '<Package>'+
              '<Text>'+xmlencode(TLocaleStrings.MsgFromIdFormat(kmcom, SKUpdate_PackageText,
                [ucr.Packages[i].Name, ucr.Packages[i].NewVersion]))+'</Text>'+
              '<NewVersion>'+xmlencode(ucr.Packages[i].NewVersion)+'</NewVersion>'+
              '<OldVersion>'+xmlencode(pkg.version)+'</OldVersion>'+
              '<DownloadSize>'+xmlencode(Format('%d', [ucr.Packages[i].DownloadSize div 1024]))+'KB</DownloadSize>'+
              '<DownloadURL>'+xmlencode(ucr.Packages[i].DownloadURL)+'</DownloadURL>'+
            '</Package>'+
          '</Update>';
        pkg := nil;
      end;
      // else Package not found, skip
    end;
  end;

  Result := '<Updates>'+xml+'</Updates>';
end;

end.

