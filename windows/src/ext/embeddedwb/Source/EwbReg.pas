//**************************************************************
//                                                             *
//                         Ewb_Reg                             *
//                                                             *
//                       For Delphi                            *
//                            by                               *
//       bsalsa - Eran Bodankin  - bsalsa@gmail.com           *
//                                                             *
//                                                             *
//  Updated versions:                                          *
//               http://www.bsalsa.com                         *
//**************************************************************
{*******************************************************************************}
{LICENSE:
THIS SOFTWARE IS PROVIDED TO YOU "AS IS" WITHOUT WARRANTY OF ANY KIND,
EITHER EXPRESSED OR IMPLIED INCLUDING BUT NOT LIMITED TO THE APPLIED
WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
YOU ASSUME THE ENTIRE RISK AS TO THE ACCURACY AND THE USE OF THE SOFTWARE
AND ALL OTHER RISK ARISING OUT OF THE USE OR PERFORMANCE OF THIS SOFTWARE
AND DOCUMENTATION. BSALSA PRODUCTIONS DOES NOT WARRANT THAT THE SOFTWARE IS ERROR-FREE
OR WILL OPERATE WITHOUT INTERRUPTION. THE SOFTWARE IS NOT DESIGNED, INTENDED
OR LICENSED FOR USE IN HAZARDOUS ENVIRONMENTS REQUIRING FAIL-SAFE CONTROLS,
INCLUDING WITHOUT LIMITATION, THE DESIGN, CONSTRUCTION, MAINTENANCE OR
OPERATION OF NUCLEAR FACILITIES, AIRCRAFT NAVIGATION OR COMMUNICATION SYSTEMS,
AIR TRAFFIC CONTROL, AND LIFE SUPPORT OR WEAPONS SYSTEMS. BSALSA PRODUCTIONS SPECIFICALLY
DISCLAIMS ANY EXPRESS OR IMPLIED WARRANTY OF FITNESS FOR SUCH PURPOSE.

You may use, change or modify the component under 4 conditions:
1. In your website, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@gmail.com) any code change in the unit
   for the benefit of the other users.
4. Please, consider donation in our web site!
{*******************************************************************************}

unit EwbReg;

interface

{$I EWB.inc}

uses
  Actions,
  Classes, {$IFDEF DELPHI6_UP}DesignEditors, DesignIntf, {$ELSE}DsgnIntf, {$ENDIF}
  EwbEditors, AppWebUpdater, IEParser, ExportFavorites, FavoritesTree, IETravelLog,
  FavMenu, FavoritesListView, FavoritesPopup, HistoryMenu, HistoryListView,
  ImportFavorites, LibXmlComps, LibXmlParser, LinksBar, RichEditBrowser,
  SecurityManager, SendMail_For_Ewb, UrlHistory, Edithost, EditDesigner,
  IEAddress, IEDownload, IEMultiDownload, EwbCore, EmbeddedWB, EwbControlComponent, IECache, Browse4Folder,
  FileExtAssociate, LinksLabel, UI_Less,
{$IFDEF DELPHI6_UP}EwbEventsComp, EwbBehaviorsComp, {$ENDIF}EwbActns;

procedure Register;

implementation

uses
  SysUtils, ActnList;

procedure Register;
begin
  RegisterComponents('Embedded Web Browser', [
    TBrowse4Folder,
{$IFDEF DELPHI6_UP}
    TEwbBehaviorFactory,
      TEwbBehaviorController,
{$ENDIF}
    TEasyXmlScanner,
      TEditDesigner,
      TEdithost,
      TEmbeddedWB,
      TEwbCore,
      TEwbControl,
      TEwbMapiMail,
      TExportFavorite,
      TFavoritesListView,
      TFavoritesMenu,
      TFavoritesPopup,
      TFavoritesTree,
      TFileExtAssociate,
      THistoryListView,
      THistoryMenu,
{$IFDEF DELPHI6_UP}
    THtmlListener,
{$ENDIF}
    TIEAddress,
      TIECache,
      TIEDownload,
      TIEMultiDownload,
      TIEParser,
      TIETravelLog,
      TImportFavorite,
      TLinksLabel,
      TLinksBar,
      TRichEditWB,
      TSecurityManager,
      TUILess,
      TUrlHistory,
      TWebUpdater,
      TXmlScanner
      ]);


  RegisterComponentEditor(TBrowse4Folder, TBFFEditor);
{$IFDEF DELPHI6_UP}
  RegisterComponentEditor(TEwbBehaviorFactory, TEwbCompEditor);
  RegisterComponentEditor(TEwbBehaviorController, TEwbCompEditor);
{$ENDIF}
  RegisterComponentEditor(TEasyXmlScanner, TEwbCompEditor);
  RegisterComponentEditor(TEditDesigner, TEwbCompEditor);
  RegisterComponentEditor(TEdithost, TEwbCompEditor);
  RegisterComponentEditor(TEmbeddedWB, TEwbCompEditor);
  RegisterComponentEditor(TEwbCore, TEwbCompEditor);
  RegisterComponentEditor(TEwbControl, TEwbCompEditor);
  RegisterComponentEditor(TEwbMapiMail, TEwbCompEditor);
  RegisterComponentEditor(TExportFavorite, TEwbCompEditor);
  RegisterComponentEditor(TFavoritesListView, TEwbCompEditor);
  RegisterComponentEditor(TFavoritesMenu, TEwbCompEditor);
  RegisterComponentEditor(TFavoritesTree, TEwbCompEditor);
  RegisterComponentEditor(TFileExtAssociate, TEwbCompEditor);
  RegisterComponentEditor(THistoryListView, TEwbCompEditor);
  RegisterComponentEditor(THistoryMenu, TEwbCompEditor);
{$IFDEF DELPHI6_UP}
  RegisterComponentEditor(THtmlListener, TEwbCompEditor);
{$ENDIF}
  RegisterComponentEditor(TIEAddress, TEwbCompEditor);
  RegisterComponentEditor(TIECache, TEwbCompEditor);
  RegisterComponentEditor(TIEDownload, TEwbCompEditor);
  RegisterComponentEditor(TIEMultiDownload, TEwbCompEditor);
  RegisterComponentEditor(TIEParser, TEwbCompEditor);
  RegisterComponentEditor(TIETravelLog, TEwbCompEditor);
  RegisterComponentEditor(TImportFavorite, TEwbCompEditor);
  RegisterComponentEditor(TLinksLabel, TEwbCompEditor);
  RegisterComponentEditor(TLinksBar, TEwbCompEditor);
  RegisterComponentEditor(TRichEditWB, TEwbCompEditor);
  RegisterComponentEditor(TSecurityManager, TEwbCompEditor);
  RegisterComponentEditor(TUILess, TEwbCompEditor);
  RegisterComponentEditor(TUrlHistory, TEwbCompEditor);
  RegisterComponentEditor(TWebUpdater, TEwbCompEditor);
  RegisterComponentEditor(TXmlScanner, TEwbCompEditor);


  RegisterPropertyEditor(TypeInfo(WideString), TIEDownload, 'DownloadDir', TBrowse4FolderDLG);
  RegisterPropertyEditor(TypeInfo(WideString), TIEParser, 'LocalFileName', TOpenFileDLG);
  RegisterPropertyEditor(TypeInfo(WideString), TIEParser, 'SaveLogAs', TSaveTextDLG);
  RegisterPropertyEditor(TypeInfo(WideString), TBrowse4Folder, 'InitialDir', TBrowse4FolderDLG);
{$IFDEF DELPHI6_UP}
  RegisterPropertyEditor(TypeInfo(WideString), TEmbeddedWB, 'HostCSS', TMultiStringProperty);
{$ENDIF}
  RegisterActions('EmbeddedWB', [TEwbLinkAction], nil);
end;

end.
