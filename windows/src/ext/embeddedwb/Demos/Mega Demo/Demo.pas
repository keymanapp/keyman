//***************************************************************
//                  EmbeddedWB Mega Demo                        *
//                                                              *
//                     Freeware Demo                            *
//                        by                                    *
//         Eran Bodankin (bsalsa) -(bsalsa@bsalsa.com)          *
//         Thanks to smot for the demo contribution             *
//                                                              *
//***************************************************************
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

You may use/ change/ modify the component under 4 conditions:
1. In your website, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@bsalsa.com) any code change in the unit  for the benefit
   of the other users.
4. Please, consider donation in our web site!
{*******************************************************************************}

unit Demo;

{$I EWB_jedi.inc}

interface

uses
  Buttons, Classes, Controls, ComCtrls, Dialogs, Forms, Graphics, Grids, Menus,
  ImgList, StdCtrls, ToolWin, ExtCtrls, OleCtrls, {$IFDEF DELPHI_7_UP}XPMan, {$ENDIF}
  sysUtils, Windows, FavMenu, ImportFavorites, ExportFavorites, Clipbrd,
  HistoryMenu, HistoryListView, FavoritesTree, FavoritesListView, EmbeddedWB,
  IEAddress, LinksBar, IEDownload, RichEditBrowser, ExtDlgs, SHDocVw_EWB,
  EwbCore, IEDownloadTools, WinSock;

type
  TfrmMain = class(TForm)
    AddCurrentSiteToTheLinksList1: TMenuItem;
    AddTheSiteToTheList1: TMenuItem;
    AddToFavorites1: TMenuItem;
    ArabicWindows1: TMenuItem;
    BalticWindows1: TMenuItem;
    btnGo: TSpeedButton;
    CacheTools1: TMenuItem;
    CentralEuropeanISO1: TMenuItem;
    CharacterSet1: TMenuItem;
    CharSetAutomatic1: TMenuItem;
    checkOnlineStatus2: TMenuItem;
    ChineseSimplifiedGB1: TMenuItem;
    ChineseTraditionalBIG1: TMenuItem;
    ClearAddressBarTypedURLs1: TMenuItem;
    ClearCache1: TMenuItem;
    ClearTheLinkList1: TMenuItem;
    ClearTheLinksList1: TMenuItem;
    Cookies1: TMenuItem;
    CookiesCheck1: TMenuItem;
    Copy: TMenuItem;
    CreateAShortCutOnYourDeskTop1: TMenuItem;
    CreateNewMail1: TMenuItem;
    Cut1: TMenuItem;
    DeleteHistory1: TMenuItem;
    MMEdit: TMenuItem;
    edtSearch: TEdit;
    EmbeddedWB1: TEmbeddedWB;
    EmbeddedWBOptions1: TMenuItem;
    Exit1: TMenuItem;
    ExportFavorite1: TExportFavorite;
    ExportFavorites1: TMenuItem;
    FavoritesListView1: TFavoritesListView;
    FavoritesMenu1: TFavoritesMenu;
    FavoritesTools1: TMenuItem;
    MMFile: TMenuItem;
    FillFormWithPersonalDetails1: TMenuItem;
    FindDialog: TMenuItem;
    GetAScreanCapture1: TMenuItem;
    GetCachedPath: TMenuItem;
    GetCookiesPath1: TMenuItem;
    GetDefaultWebBrowserFromResistry1: TMenuItem;
    GetFavoritesPath1: TMenuItem;
    GetHistoryPath1: TMenuItem;
    GetHostAndIP1: TMenuItem;
    GetIEHomePage: TMenuItem;
    GetJpegPageCapture1: TMenuItem;
    GetSpecialFolderPath1: TMenuItem;
    GetThumbnail: TMenuItem;
    GoAboutBlank1: TMenuItem;
    GoBack1: TMenuItem;
    GoDowloadMasked1: TMenuItem;
    Godownloadafile1: TMenuItem;
    GoForward1: TMenuItem;
    GoHome1: TMenuItem;
    GoSearch1: TMenuItem;
    GoWithQueryDetails1: TMenuItem;
    GreekWindows1: TMenuItem;
    GroupBox1: TGroupBox;
    haiWindows1: TMenuItem;
    HistoryMenu: THistoryMenu;
    HistoryTools1: TMenuItem;
    HomePage1: TMenuItem;
    ilFavoritesTree: TImageList;
    ilToolBar: TImageList;
    Images1: TMenuItem;
    ImageViewer: TImage;
    ImgSSL: TImage;
    imgUn: TImage;
    imgZone: TImage;
    ImportExportWizard1: TMenuItem;
    ImportFavorite1: TImportFavorite;
    ImportFavorites1: TMenuItem;
    InternetOptions1: TMenuItem;
    Korean1: TMenuItem;
    Large1: TMenuItem;
    Largest1: TMenuItem;
    lblLevel: TLabel;
    lblSSL: TLabel;
    lblZone: TLabel;
    LinksBar1: TLinksBar;
    LinksList1: TMenuItem;
    LoadFromStream: TMenuItem;
    LoadFromStrings: TMenuItem;
    lvEventLog: TListBox;
    MailTools1: TMenuItem;
    MainMenu1: TMainMenu;
    MainToolBar: TToolBar;
    Medium1: TMenuItem;
    N1: TMenuItem;
    N10: TMenuItem;
    N11: TMenuItem;
    N12: TMenuItem;
    N13: TMenuItem;
    N14: TMenuItem;
    N15: TMenuItem;
    N16: TMenuItem;
    N17: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    N9: TMenuItem;
    NavigateToLinkItem1: TMenuItem;
    NavigateToLinkListItem1: TMenuItem;
    MMNavigation: TMenuItem;
    OfflineMode1: TMenuItem;
    Open: TMenuItem;
    Open1: TMenuItem;
    OpenAddressBook1: TMenuItem;
    OpenCalender1: TMenuItem;
    OpenFoldersExplore1: TMenuItem;
    OpenGoogleMail1: TMenuItem;
    OpenHotmailMail1: TMenuItem;
    OpenNetMeeting: TMenuItem;
    OpenNewsClient1: TMenuItem;
    OpenOtherBrowsersFavorites1: TMenuItem;
    OpenOutlook1: TMenuItem;
    OpenOutlookExpress1: TMenuItem;
    OpenRegistryEditor1: TMenuItem;
    OpenYahooMail1: TMenuItem;
    PageCtrl: TPageControl;
    PageSetup1: TMenuItem;
    PageSourceHtmlasstrings1: TMenuItem;
    PageSourceText: TMenuItem;
    PageSourceTextasstrings1: TMenuItem;
    PaneAddress: TPanel;
    PanelAcc: TPanel;
    PanelMiddle: TPanel;
    PanelSearch: TPanel;
    PanelSecurity: TPanel;
    PanelTop: TPanel;
    PanelWEB: TPanel;
    Pastefromclipboard1: TMenuItem;
    pctrlWB: TPageControl;
    pnlFav: TPanel;
    PopupMenu1: TPopupMenu;
    Print: TMenuItem;
    PrintPreview1: TMenuItem;
    PrintWithOptions: TMenuItem;
    ProgressBar1: TProgressBar;
    Properties: TMenuItem;
    Refresh1: TMenuItem;
    RemoveTheCurrentSiteFromTheLinksList1: TMenuItem;
    RemoveTheSiteFromTheList1: TMenuItem;
    SaveAllImages: TMenuItem;
    SaveAs1: TMenuItem;
    Savepagetext: TMenuItem;
    SavePageToEmail2: TMenuItem;
    SavePageToStream: TMenuItem;
    SavePageToStrings1: TMenuItem;
    SaveThePageTofile1: TMenuItem;
    sbRebuildView: TSpeedButton;
    Scrolltothebottom1: TMenuItem;
    ScrollToTheTop1: TMenuItem;
    ScrolToPosition1: TMenuItem;
    SearchImMsn1: TMenuItem;
    SearchInGoogle1: TMenuItem;
    SearchInYahoo1: TMenuItem;
    SelectAll: TMenuItem;
    SendThePageInMail1: TMenuItem;
    SendTheURL: TMenuItem;
    SetIENewHomePage: TMenuItem;
    ShowHideTheLinksToolbar1: TMenuItem;
    ShowInternetExplorerVersion1: TMenuItem;
    ShowOrganizeFavorites1: TMenuItem;
    ShowTheList1: TMenuItem;
    ShowTheList2: TMenuItem;
    Small1: TMenuItem;
    Smallest1: TMenuItem;
    Spacer: TToolButton;
    spdBtnGoogleSearch: TSpeedButton;
    Splitter2: TSplitter;
    SplitterACC: TSplitter;
    Stop1: TMenuItem;
    TabBrowser: TTabSheet;
    TabEditor: TTabSheet;
    TabEvents: TTabSheet;
    TabFavoritesLV: TTabSheet;
    TabFavoritesTV: TTabSheet;
    TabHistoryLV: TTabSheet;
    TabImage: TTabSheet;
    tbBlock: TToolButton;
    ToolBtnAccesories: TToolButton;
    ToolbtnBack: TToolButton;
    ToolBtnForward: TToolButton;
    ToolBtnHome: TToolButton;
    ToolBtnRefresh: TToolButton;
    ToolBtnSearch: TToolButton;
    ToolBtnStop: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    MMTools: TMenuItem;
    UnicodeUTF1: TMenuItem;
    urkishWindows1: TMenuItem;
    MMView: TMenuItem;
    ViewHidethelinksbar1: TMenuItem;
    ViewHideTheLinksToolbar1: TMenuItem;
    ViewPageImagesAsAList1: TMenuItem;
    ViewSourceHtml: TMenuItem;
    WesternEuropeanISO1: TMenuItem;
    Zoom1: TMenuItem;
    Security1: TMenuItem;
    AddToRestrictedZoneList: TMenuItem;
    CheckIfInResrictedZoneList1: TMenuItem;
    N18: TMenuItem;
    AddToTrustedListZoneList1: TMenuItem;
    CheckIfInTrustedZoneList1: TMenuItem;
    N19: TMenuItem;
    CheckSiteSecurityLevel1: TMenuItem;
    CheckSiteSecurityZone1: TMenuItem;
    FavoritesTree1: TFavoritesTree;
    HistoryListView1: THistoryListView;
    TabLinks: TTabSheet;
    IEDownload1: TIEDownload;
    PanelLinksTop: TPanel;
    StringGrid1: TStringGrid;
    PanelLinksBottum: TPanel;
    Button1: TButton;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    CheckTheLinks: TMenuItem;
    N20: TMenuItem;
    IEDownload: TMenuItem;
    CheckURLsFromALinkList1: TMenuItem;
    TabFTP: TTabSheet;
    EmbeddedWB2: TEmbeddedWB;
    Panel1: TPanel;
    sbRefresh: TSpeedButton;
    edtUser: TEdit;
    edtPassword: TEdit;
    sbConnect: TSpeedButton;
    sbUp: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    SetDesignMode: TMenuItem;
    IEAddress1: TIEAddress;
    RichViewWBDemo1: TMenuItem;
    HighlighHTML1: TMenuItem;
    HighLightXML1: TMenuItem;
    HighLightURL1: TMenuItem;
    InsertFromImageList: TMenuItem;
    SetSelectionAsAHyperLink1: TMenuItem;
    SetWordAsAHyperLink1: TMenuItem;
    AddBullets1: TMenuItem;
    AddLineNumbers1: TMenuItem;
    AddRomanNumbers1: TMenuItem;
    SetFontColor: TMenuItem;
    New1: TMenuItem;
    Open2: TMenuItem;
    Save1: TMenuItem;
    SaveAs2: TMenuItem;
    AddBackroundColor1: TMenuItem;
    SelectFonts1: TMenuItem;
    SetSize1: TMenuItem;
    SetBold1: TMenuItem;
    SetItalic1: TMenuItem;
    SetUnderLine1: TMenuItem;
    AddDateAndTime1: TMenuItem;
    PreviewRichEditLinesInTheBrowser1: TMenuItem;
    LoadCodeFromBrowserStream1: TMenuItem;
    SetColor1: TMenuItem;
    Print1: TMenuItem;
    Find1: TMenuItem;
    Replace1: TMenuItem;
    CreateASnapshot1: TMenuItem;
    AddACheckBox1: TMenuItem;
    N27: TMenuItem;
    AddTEditBox1: TMenuItem;
    AddAButton1: TMenuItem;
    AddARadioButton1: TMenuItem;
    GoToLineNumber1: TMenuItem;
    WrapLongLines1: TMenuItem;
    MailSelectedText1: TMenuItem;
    Mail1: TMenuItem;
    ProgressBar2: TProgressBar;
    ProgressBar3: TProgressBar;
    ilsSmilies: TImageList;
    InsertFile: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    OpenDialog1: TOpenDialog;
    hemes1: TMenuItem;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    File1: TMenuItem;
    Edit: TMenuItem;
    Tools: TMenuItem;
    N22: TMenuItem;
    N23: TMenuItem;
    N24: TMenuItem;
    N25: TMenuItem;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ShowTheEditor2: TMenuItem;
    Fonts: TMenuItem;
    Add: TMenuItem;
    N26: TMenuItem;
    N21: TMenuItem;
    InsertBitmap: TMenuItem;
    N28: TMenuItem;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ResetFontsFormat1: TMenuItem;
    ShowTheImageEditor1: TMenuItem;
    stBar: TStatusBar;
    miSave: TMenuItem;
    Label3: TLabel;
    RichEditWB1: TRichEditWB;
    procedure EmbeddedWB1Refresh(Sender: TCustomEmbeddedWB; CmdID: Integer;
      var Cancel: Boolean);
    procedure EmbeddedWB1ScriptError(Sender: TObject; ErrorLine, ErrorCharacter,
      ErrorCode, ErrorMessage, ErrorUrl: string;
      var ScriptErrorAction: TScriptErrorAction);
    procedure FavoritesMenu1AddFavorites(const EmbeddedWB: TEmbeddedWB; Title,
      URL: WideString; Success: Integer);
    procedure miSaveClick(Sender: TObject);
    procedure IEDownload1Progress(Sender: TBSCB; ulProgress, ulProgressMax,
      ulStatusCode: Cardinal; szStatusText: PWideChar; Downloaded, ElapsedTime,
      Speed, RemainingTime, Status: string);
    procedure ShowTheImageEditor1Click(Sender: TObject);
    procedure ResetFontsFormat1Click(Sender: TObject);
    procedure InsertBitmapClick(Sender: TObject);
    procedure ShowTheEditor2Click(Sender: TObject);
    procedure hemes1Click(Sender: TObject);
    procedure InsertFileClick(Sender: TObject);
    procedure RichEditWB1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure RichEditWB1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Mail1Click(Sender: TObject);
    procedure MailSelectedText1Click(Sender: TObject);
    procedure WrapLongLines1Click(Sender: TObject);
    procedure GoToLineNumber1Click(Sender: TObject);
    procedure AddAButton1Click(Sender: TObject);
    procedure AddARadioButton1Click(Sender: TObject);
    procedure AddTEditBox1Click(Sender: TObject);
    procedure AddACheckBox1Click(Sender: TObject);
    procedure CreateASnapshot1Click(Sender: TObject);
    procedure Replace1Click(Sender: TObject);
    procedure Find1Click(Sender: TObject);
    procedure Print1Click(Sender: TObject);
    procedure SetColor1Click(Sender: TObject);
    procedure LoadCodeFromBrowserStream1Click(Sender: TObject);
    procedure PreviewRichEditLinesInTheBrowser1Click(Sender: TObject);
    procedure AddDateAndTime1Click(Sender: TObject);
    procedure SetUnderLine1Click(Sender: TObject);
    procedure SetItalic1Click(Sender: TObject);
    procedure SetSize2Click(Sender: TObject);
    procedure SetBold1Click(Sender: TObject);
    procedure SetSize1Click(Sender: TObject);
    procedure SelectFonts1Click(Sender: TObject);
    procedure AddBackroundColor1Click(Sender: TObject);
    procedure SetFontColorClick(Sender: TObject);
    procedure SaveAs2Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Open2Click(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure AddRomanNumbers1Click(Sender: TObject);
    procedure AddLineNumbers1Click(Sender: TObject);
    procedure AddBullets1Click(Sender: TObject);
    procedure SetWordAsAHyperLink1Click(Sender: TObject);
    procedure SetSelectionAsAHyperLink1Click(Sender: TObject);
    procedure InsertFromImageListClick(Sender: TObject);
    procedure HighLightXML1Click(Sender: TObject);
    procedure HighLightURL1Click(Sender: TObject);
    procedure HighlighHTML1Click(Sender: TObject);
    procedure SetDesignModeClick(Sender: TObject);
    procedure EmbeddedWB2CommandStateChange(ASender: TObject; Command: Integer;
      Enable: WordBool);
    procedure sbConnectClick(Sender: TObject);
    procedure sbUpClick(Sender: TObject);
    procedure sbRefreshClick(Sender: TObject);
    procedure EmbeddedWB1SetSecureLockIcon(ASender: TObject;
      SecureLockIcon: Integer);
    procedure CheckURLsFromALinkList1Click(Sender: TObject);
    procedure CheckTheLinksClick(Sender: TObject);
    procedure RadioButton2Click(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    function IEDownload1Response(Sender: TBSCB; dwResponseCode: Cardinal;
      szResponseHeaders, szRequestHeaders: PWideChar;
      out szAdditionalRequestHeaders: PWideChar): HRESULT;
    procedure Button1Click(Sender: TObject);
    procedure FavoritesTree1NodeAdded(Sender: TObject; const aNode: TTreeNode;
      aNodeType: TNodeType);
    procedure FavoritesTree1Expanded(Sender: TObject; Node: TTreeNode);
    procedure CheckPageSecurityEncryption1Click(Sender: TObject);
    procedure CheckSiteSecurityZone1Click(Sender: TObject);
    procedure CheckSiteSecurityLevel1Click(Sender: TObject);
    procedure CheckIfInTrustedZoneList1Click(Sender: TObject);
    procedure AddToTrustedListZoneList1Click(Sender: TObject);
    procedure CheckIfInResrictedZoneList1Click(Sender: TObject);
    procedure AddToRestrictedZoneListClick(Sender: TObject);
    procedure AddTheSiteToTheList1Click(Sender: TObject);
    procedure AddToFavorites1Click(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    procedure CharSetAutomatic1Click(Sender: TObject);
    procedure checkOnlineStatus2Click(Sender: TObject);
    procedure ClearAddressBarTypedURLs1Click(Sender: TObject);
    procedure ClearCache1Click(Sender: TObject);
    procedure ClearTheLinkList1Click(Sender: TObject);
    procedure CookiesCheck1Click(Sender: TObject);
    procedure CopyClick(Sender: TObject);
    procedure CreateAShortCutOnYourDeskTop1Click(Sender: TObject);
    procedure CreateNewMail1Click(Sender: TObject);
    procedure Cut1Click(Sender: TObject);
    procedure DeleteHistory1Click(Sender: TObject);
    procedure EmbeddedWB1BeforeNavigate2(ASender: TObject; const pDisp: IDispatch;
      var URL, Flags, TargetFrameName, PostData, Headers: OleVariant; var Cancel: WordBool);
    procedure EmbeddedWB1CommandStateChange(ASender: TObject; Command: Integer;
      Enable: WordBool);
    procedure EmbeddedWB1DocumentComplete(ASender: TObject; const pDisp: IDispatch;
      var URL: OleVariant);
    procedure EmbeddedWB1DownloadBegin(Sender: TObject);
    procedure EmbeddedWB1DownloadComplete(Sender: TObject);
    procedure EmbeddedWB1FullScreen(ASender: TObject; FullScreen: WordBool);
    procedure EmbeddedWB1NavigateComplete2(ASender: TObject; const pDisp: IDispatch;
      var URL: OleVariant);
    procedure EmbeddedWB1NewWindow2(ASender: TObject; var ppDisp: IDispatch;
      var Cancel: WordBool);
    procedure EmbeddedWB1ProgressChange(ASender: TObject; Progress, ProgressMax: Integer);
    procedure EmbeddedWB1PropertyChange(ASender: TObject; const szProperty: WideString);
    procedure EmbeddedWB1StatusTextChange(ASender: TObject; const Text: WideString);
    procedure EmbeddedWB1TitleChange(ASender: TObject; const Text: WideString);
    procedure EmbeddedWB1Visible(ASender: TObject; Visible: WordBool);
    procedure Exit1Click(Sender: TObject);
    procedure ExportFavorites1Click(Sender: TObject);
    procedure FillFormWithPersonalDetails1Click(Sender: TObject);
    procedure FindDialogClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GetAScreanCapture1Click(Sender: TObject);
    procedure GetCachedPathClick(Sender: TObject);
    procedure GetCookiesPath1Click(Sender: TObject);
    procedure GetDefaultWebBrowserFromResistry1Click(Sender: TObject);
    procedure GetFavoritesPath1Click(Sender: TObject);
    procedure GetHistoryPath1Click(Sender: TObject);
    procedure GetHostAndIP1Click(Sender: TObject);
    procedure GetIEHomePageClick(Sender: TObject);
    procedure GetJpegPageCapture1Click(Sender: TObject);
    procedure GetSpecialFolderPath1Click(Sender: TObject);
    procedure GetThumbnailClick(Sender: TObject);
    procedure GoAboutBlank1Click(Sender: TObject);
    procedure GoDowloadMasked1Click(Sender: TObject);
    procedure Godownloadafile1Click(Sender: TObject);
    procedure GoWithQueryDetails1Click(Sender: TObject);
    procedure IEAddress1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ImportExportWizard1Click(Sender: TObject);
    procedure ImportFavorites1Click(Sender: TObject);
    procedure InternetOptions1Click(Sender: TObject);
    procedure LoadFromStreamClick(Sender: TObject);
    procedure LoadFromStringsClick(Sender: TObject);
    procedure NavigateToLinkItem1Click(Sender: TObject);
    procedure OfflineMode1Click(Sender: TObject);
    procedure OpenAddressBook1Click(Sender: TObject);
    procedure OpenCalender1Click(Sender: TObject);
    procedure OpenClick(Sender: TObject);
    procedure OpenFoldersExplore1Click(Sender: TObject);
    procedure OpenGoogleMail1Click(Sender: TObject);
    procedure OpenHotmailMail1Click(Sender: TObject);
    procedure OpenMailClient1Click(Sender: TObject);
    procedure OpenNetMeetingClick(Sender: TObject);
    procedure OpenNewsClient1Click(Sender: TObject);
    procedure OpenOutlook1Click(Sender: TObject);
    procedure OpenOutlookExpress1Click(Sender: TObject);
    procedure OpenRegistryEditor1Click(Sender: TObject);
    procedure OpenYahooMail1Click(Sender: TObject);
    procedure PageCtrlChange(Sender: TObject);
    procedure PageSetup1Click(Sender: TObject);
    procedure PageSourceHtmlasstrings1Click(Sender: TObject);
    procedure PageSourceTextasstrings1Click(Sender: TObject);
    procedure PageSourceTextClick(Sender: TObject);
    procedure Pastefromclipboard1Click(Sender: TObject);
    procedure PrintClick(Sender: TObject);
    procedure PrintPreview1Click(Sender: TObject);
    procedure PrintWithOptionsClick(Sender: TObject);
    procedure PropertiesClick(Sender: TObject);
    procedure RemoveTheSiteFromTheList1Click(Sender: TObject);
    procedure SaveAllImagesClick(Sender: TObject);
    procedure SaveAs1Click(Sender: TObject);
    procedure SavepagetextClick(Sender: TObject);
    procedure SavePageToStreamClick(Sender: TObject);
    procedure SavePageToStrings1Click(Sender: TObject);
    procedure SaveThePageTofile1Click(Sender: TObject);
    procedure sbRebuildViewClick(Sender: TObject);
    procedure Scrolltothebottom1Click(Sender: TObject);
    procedure ScrollToTheTop1Click(Sender: TObject);
    procedure ScrolToPosition1Click(Sender: TObject);
    procedure SearchImMsn1Click(Sender: TObject);
    procedure SearchInGoogle1Click(Sender: TObject);
    procedure SearchInYahoo1Click(Sender: TObject);
    procedure SelectAllClick(Sender: TObject);
    procedure SendThePageInMail1Click(Sender: TObject);
    procedure SendTheURLClick(Sender: TObject);
    procedure SetIENewHomePageClick(Sender: TObject);
    procedure ShowInternetExplorerVersion1Click(Sender: TObject);
    procedure ShowOrganizeFavorites1Click(Sender: TObject);
    procedure ShowTheList1Click(Sender: TObject);
    procedure Smallest1Click(Sender: TObject);
    procedure spdBtnGoogleSearchClick(Sender: TObject);
    procedure ToolBtnAccesoriesClick(Sender: TObject);
    procedure ToolbtnBackClick(Sender: TObject);
    procedure ToolBtnForwardClick(Sender: TObject);
    procedure ToolBtnHomeClick(Sender: TObject);
    procedure ToolBtnRefreshClick(Sender: TObject);
    procedure ToolBtnSearchClick(Sender: TObject);
    procedure ToolBtnStopClick(Sender: TObject);
    procedure ViewHidethelinksbar1Click(Sender: TObject);
    procedure ViewHideTheLinksToolbar1Click(Sender: TObject);
    procedure ViewPageLinksAsAList1Click(Sender: TObject);
    procedure ViewSourceHtmlClick(Sender: TObject);
    procedure Zoom1Click(Sender: TObject);
    procedure edtSearchDblClick(Sender: TObject);
    procedure EmbeddedWB1ShowDialog(Sender: TObject; h: Cardinal;
      StyleEx: Integer; OldCaption: string; var NewCaption: WideString;
      var Cancel: Boolean);
  private
    Links: TStringlist;
    EncryptionSt: string;
    InitialURL: WideString;
    procedure AddEventLog(s: string);
    procedure UpdateSecurityZone;
    procedure UpdateSSLStatus;
    procedure UpdateStopButton;
    procedure UpdateControls;
    procedure UpdateEditor;
    procedure UpdateImage;
    procedure UpdateView;
    procedure ClearResultsGrid;
    procedure ClearAllGrid;
    procedure UpdateLinksChecker;
    procedure UpdateProgressBars;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  EWBTools;

//////////////////{ Private Section } //////////////////

procedure TfrmMain.UpdateProgressBars;
begin
  ProgressBar2.Max := RichEditWB1.GetLineCount;
  ProgressBar2.Position := RichEditWB1.CursorPositionY;
  ProgressBar3.Max := RichEditWB1.GetLineLength(RichEditWB1.CursorPositionY);
  ProgressBar3.Position := RichEditWB1.CursorPositionX;
end;

procedure TfrmMain.UpdateSecurityZone;
var
  Name, Description: string;
  Icon: TIcon;
begin
  Icon := TIcon.Create;
  try
    EmbeddedWB1.GetURLSecurityZone(Name, Description, Icon);
    imgZone.Picture.Icon := Icon;
    Forms.Application.Icon := Icon;
  finally
    Icon.Free;
  end;
  stBar.Panels[1].Text := Name;
  stBar.Hint := Description;
  lblZone.Caption := Name;
  lblZone.Hint := Description;
  imgZone.Hint := 'The Web Site Security Zone Is ' + Name;
end;

procedure TfrmMain.UpdateSSLStatus;
var
  SSLName, SSLDescription: string;
begin
  if EmbeddedWB1.GetSSLStatus(SSLName, SSLDescription) then
  begin
    ImgUn.Visible := False;
    imgSSl.Visible := True;
    stBar.Panels[2].Text := SSLName;
    stBar.Hint := SSLDescription;
    lblSSL.Caption := SSLName;
    lblSSL.Hint := SSLDescription;
    imgSSL.Hint := 'The Page SSL Security Status Is ' + SSLName;
  end
  else
  begin
    ImgUn.Visible := True;
    stBar.Panels[2].Text := '';
    stBar.Hint := '';
    lblSSL.Caption := '';
    lblSSL.Hint := '';
    imgSSl.Visible := False;
    imgSSL.Hint := '';
  end;
end;

procedure TfrmMain.AddEventLog(s: string);
begin
  lvEventLog.ItemIndex := lvEventLog.Items.Add(s);
end;

procedure TfrmMain.UpdateView;
begin
  case pagectrl.ActivePageIndex of
    0: FavoritesListView1.Loaded;
    1: FavoritesTree1.Refresh;
    2: HistoryListView1.Loaded;
    4: EmbeddedWB2.Navigate('c:\');
  end
end;

procedure TfrmMain.UpdateEditor;
begin
  pctrlWB.Pages[1].TabVisible := True;
  pctrlWB.ActivePage := TabEditor;
end;

procedure TfrmMain.UpdateLinksChecker;
begin
  pctrlWB.Pages[3].TabVisible := True;
  pctrlWB.ActivePage := TabLinks;
end;

procedure TfrmMain.UpdateImage;
begin
  pctrlWB.Pages[2].TabVisible := True;
  pctrlWB.ActivePage := TabImage;
end;

procedure TfrmMain.UpdateStopButton;
begin
  if EmbeddedWB1.Busy then
  begin
    ToolBtnStop.Enabled := True;
    Stop1.Enabled := True;
  end
  else
  begin
    ToolBtnStop.Enabled := False;
    Stop1.Enabled := False;
  end;
end;

procedure TfrmMain.UpdateControls;
begin
 if Assigned(EmbeddedWB1) then
  if EmbeddedWB1.Busy then
    EmbeddedWB1.Stop;
end;

procedure TfrmMain.ClearResultsGrid;
var
  X, y: Integer;
begin
  for x := 1 to Links.Count do
    for y := 1 to 2 do
      Stringgrid1.Cells[y, x] := '';
end;

procedure TfrmMain.ClearAllGrid;
var
  X, y: Integer;
begin
  for x := 1 to Links.Count do
    for y := 0 to 2 do
      Stringgrid1.Cells[y, x] := '';
end;

////////////{ End of private section } ///////////

//////////////////{ Form procedures section} //////////////////

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Links.Free;
  HistoryMenu.Free;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  UrlList: TStringList;
begin
// RichEditWB1.RTFText := '{\rtf1\ansi\deff0{\fonttbl{\f0\fnil Tahoma;}}{\colortbl ;\red0\green0\blue0;}\viewkind4\uc1\pard\cf1\lang2055\f0\fs16\par}';

{$IFDEF DELPHI_7_UP}
  TxpManifest.Create(FrmMain);
{$ENDIF}
  HistoryMenu.CreateMenu;
  FavoritesMenu1.CreateMenu;
  UrlList := TStringList.Create;
  try
    Links := TStringlist.create;
    try
      if FileExists('links.txt') then
        Links.LoadFromFile('links.txt');
      LinksBar1.GetTheLinksURLs(UrlList);
      Links.Add('');
      Links.AddStrings(UrlList);
    finally
    end;
  finally
    UrlList.Free;
  end;
end;

procedure TfrmMain.FormShow(Sender: TObject);
var
  LastVisitedPage: string;
  i: Integer;
begin
{$IFDEF DELPHI_9_UP}
  EmbeddedWB1.OnSetSecureLockIcon := EmbeddedWB1SetSecureLockIcon;
{$ENDIF}
  EmbeddedWB2.Go('c:\');
  Stringgrid1.Cells[0, 0] := 'Url';
  Stringgrid1.Cells[1, 0] := 'Status';
  Stringgrid1.Cells[2, 0] := 'Result';
  for i := 0 to links.count - 1 do
  begin
    Stringgrid1.RowCount := i + 2;
    Stringgrid1.Cells[0, i + 1] := links[i];
  end;
  pctrlWB.ActivePage := TabBrowser;
  EmbeddedWB1.RestoreApplicationFormSize;
  if EmbeddedWB1.GetLastVisitedPage(LastVisitedPage) and
    (Pos('http', LastVisitedPage) > 0) then
    EmbeddedWB1.Navigate(LastVisitedPage)
  else
    EmbeddedWB1.GoHome;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  EmbeddedWB1.SaveLastVisitedPage;
  EmbeddedWB1.SaveApplicationFormSize;
  UpdateControls;
  if FileExists(ExtractFilePath(ParamStr(0)) + 'Site.html') then
    DeleteFile('Site.html');
  CanClose := Assigned(EmbeddedWB1) and not EmbeddedWB1.Busy
//  if canclose then asm int 3;  end;
end;
//////////////////{End of Form procedures section } //////////////////

procedure TfrmMain.sbRebuildViewClick(Sender: TObject);
begin
  UpdateView();
end;

procedure TfrmMain.OpenClick(Sender: TObject);
begin
  EmbeddedWB1.OpenDialog;
end;

procedure TfrmMain.PrintClick(Sender: TObject);
begin
  EmbeddedWB1.Print;
end;

procedure TfrmMain.PrintPreview1Click(Sender: TObject);
begin
  EmbeddedWB1.PrintPreView;
end;

procedure TfrmMain.PageSetup1Click(Sender: TObject);
begin
  EmbeddedWB1.PrintSetup;
end;

procedure TfrmMain.PropertiesClick(Sender: TObject);
begin
  EmbeddedWB1.ShowPageProperties;
end;

procedure TfrmMain.OfflineMode1Click(Sender: TObject);
begin
  if OfflineMode1.Checked then
    EmbeddedWB1.WorkOffline
  else
    EmbeddedWB1.WorkOnline;
end;

procedure TfrmMain.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.SaveAs1Click(Sender: TObject);
begin
  EmbeddedWB1.SaveDialog;
end;

procedure TfrmMain.SavepagetextClick(Sender: TObject);
begin
  EmbeddedWB1.SavePageTextDialog;
end;

procedure TfrmMain.ViewSourceHtmlClick(Sender: TObject);
begin
  EmbeddedWB1.ViewPageSourceHtml;
end;

procedure TfrmMain.PageSourceTextClick(Sender: TObject);
begin
  EmbeddedWB1.ViewPageSourceText;
end;

procedure TfrmMain.InternetOptions1Click(Sender: TObject);
begin
  EmbeddedWB1.ShowInternetOptions;
end;

procedure TfrmMain.CopyClick(Sender: TObject);
begin
  EmbeddedWB1.Copy;
end;

procedure TfrmMain.SelectAllClick(Sender: TObject);
begin
  EmbeddedWB1.SelectAll;
end;

procedure TfrmMain.FindDialogClick(Sender: TObject);
begin
  EmbeddedWB1.ShowFindDialog;
end;

procedure TfrmMain.SaveAllImagesClick(Sender: TObject);
begin
  EmbeddedWB1.SaveImagesDialog;
end;

procedure TfrmMain.GetIEHomePageClick(Sender: TObject);
begin
  ShowMessage(EmbeddedWB1.GetIEHomePage);
end;

procedure TfrmMain.SetIENewHomePageClick(Sender: TObject);
begin
  EmbeddedWB1.SetNewHomePage('http://groups.yahoo.com/group/delphi-webbrowser/');
end;

procedure TfrmMain.SendTheURLClick(Sender: TObject);
begin
  EmbeddedWB1.SendURLInMail;
end;

procedure TfrmMain.EmbeddedWB1DownloadComplete(Sender: TObject);
begin
  AddEventLog('OnDocumentComplete ' + EmbeddedWB1.LocationName);
  ProgressBar1.Visible := False;
  EmbeddedWB1.SetFocusToDoc;
end;

procedure TfrmMain.EmbeddedWB1TitleChange(ASender: TObject;
  const Text: WideString);
begin
  frmMain.Caption := ('Web Site Name: ' + EmbeddedWB1.LocationName);
  stBar.Panels[0].Text := Text;
end;

procedure TfrmMain.EmbeddedWB1StatusTextChange(ASender: TObject;
  const Text: WideString);
begin
  stBar.Panels[0].Text := Text;
end;

procedure TfrmMain.ToolBtnRefreshClick(Sender: TObject);
begin
  UpdateControls;
  EmbeddedWB1.Refresh;
end;

procedure TfrmMain.ToolBtnHomeClick(Sender: TObject);
begin
  UpdateControls;
  EmbeddedWB1.GoHome;
end;

procedure TfrmMain.ToolBtnSearchClick(Sender: TObject);
begin
  EmbeddedWB1.ShowFindDialog;
end;

procedure TfrmMain.ToolBtnAccesoriesClick(Sender: TObject);
begin
  PanelACC.Visible := ToolBtnAccesories.Down;
end;

procedure TfrmMain.ToolBtnStopClick(Sender: TObject);
begin
  UpdateControls;
end;

procedure TfrmMain.ToolBtnForwardClick(Sender: TObject);
begin
  UpdateControls;
  EmbeddedWB1.GoForward;
end;

procedure TfrmMain.ToolbtnBackClick(Sender: TObject);
begin
  UpdateControls;
  EmbeddedWB1.GoBack;
end;

procedure TfrmMain.btnGoClick(Sender: TObject);
begin
  UpdateControls;
  EmbeddedWB1.Go(Trim(IEAddress1.Text));
end;

procedure TfrmMain.EmbeddedWB1CommandStateChange(ASender: TObject;
  Command: Integer; Enable: WordBool);
const
  CSC_UPDATECOMMANDS = -1;
begin
  case Command of
    CSC_NAVIGATEBACK:
      begin
        ToolbtnBack.Enabled := Enable;
        GoBack1.Enabled := Enable;
      end;
    CSC_NAVIGATEFORWARD:
      begin
        ToolbtnForward.Enabled := Enable;
        GoForward1.Enabled := Enable;
      end;
    CSC_UPDATECOMMANDS:
      begin
        ToolbtnStop.Enabled := EmbeddedWB1.Busy;
        Stop1.Enabled := EmbeddedWB1.Busy;
      end;
  end;
end;

procedure TfrmMain.EmbeddedWB1DownloadBegin(Sender: TObject);
begin
  AddEventLog('OnDownloadBegin');
  UpdateStopButton;
end;

procedure TfrmMain.EmbeddedWB1PropertyChange(ASender: TObject;
  const szProperty: WideString);
begin
  UpdateStopButton;
end;

procedure TfrmMain.EmbeddedWB1Refresh(Sender: TCustomEmbeddedWB; CmdID: Integer;
  var Cancel: Boolean);
begin
  AddEventLog('OnRefresh. CmdID: ' + IntToStr(CmdID));
end;

procedure TfrmMain.EmbeddedWB1ProgressChange(ASender: TObject; Progress,
  ProgressMax: Integer);
begin
  UpdateStopButton;
  if (Progress >= 1) and (ProgressMax > 1) then
  begin
    ProgressBar1.Position := Round((Progress * 100) div ProgressMax);
    ProgressBar1.Visible := True;
    // Stbar.Panels[3].Text  := 'Download Progress: '+ Format('%d %', [Progress Div 100]);
  end else
  begin
    ProgressBar1.Position := 1;
    //  stBar.Panels[3].Text:='';
    ProgressBar1.Visible := False;
  end;
end;

procedure TfrmMain.Smallest1Click(Sender: TObject);
var
  k: Integer;
begin
  if not EmbeddedWB1.Busy then
  begin
    for k := 0 to Zoom1.Count - 1 do
      Zoom1.Items[k].Checked := False;

    TMenuItem(Sender).Checked := True;
    EmbeddedWb1.Zoom := ((Sender as TMenuitem).Tag);
  end;
end;

procedure TfrmMain.Zoom1Click(Sender: TObject);
var
  ZoomIndex: integer;
begin
  ZoomIndex := EmbeddedWB1.Zoom;
  case ZoomIndex of
    4: Largest1.Checked := True;
    3: Large1.Checked := True;
    2: Medium1.Checked := True;
    1: Small1.Checked := True;
    0: Smallest1.Checked := True;
  end;
end;

procedure TfrmMain.Cut1Click(Sender: TObject);
begin
  EmbeddedWB1.Cut;
end;

procedure TfrmMain.Pastefromclipboard1Click(Sender: TObject);
begin
  EmbeddedWB1.Paste;
end;

procedure TfrmMain.ScrollToTheTop1Click(Sender: TObject);
begin
  EmbeddedWB1.ScrollToTop;
end;

procedure TfrmMain.Scrolltothebottom1Click(Sender: TObject);
begin
  EmbeddedWB1.ScrollToBottom;
end;

procedure TfrmMain.ScrolToPosition1Click(Sender: TObject);
begin
  EmbeddedWB1.ScrollToPosition(200, 200);
end;

procedure TfrmMain.GetCachedPathClick(Sender: TObject);
begin
  ShowMessage(EmbeddedWB1.GetCachedFileFromURL(IEAddress1.Text));
end;

procedure TfrmMain.CharSetAutomatic1Click(Sender: TObject);
begin
  if EmbeddedWB1.SetCharartersSet(TMenuItem(Sender).Hint) then
    TMenuItem(Sender).Checked := True;
end;

procedure TfrmMain.CookiesCheck1Click(Sender: TObject);
var
  st: string;
begin
  st := EmbeddedWB1.GetCookie;
  SetLength(st, 180);
  if st <> '' then
    ShowMessage('The web site has just added a cookie:'
      + #10 + #13 + st)
  else
    ShowMessage('No cookies were found.');
end;

procedure TfrmMain.spdBtnGoogleSearchClick(Sender: TObject);
begin
  EmbeddedWB1.GoSearchInGoogle(edtSearch.Text);
end;

procedure TfrmMain.edtSearchDblClick(Sender: TObject);
begin
  edtSearch.SelectAll;
end;

procedure TfrmMain.GetCookiesPath1Click(Sender: TObject);
var
  st: string;
begin
  st := EmbeddedWB1.GetCookiesPath;
  ShowMessage(st);
end;

procedure TfrmMain.GetHistoryPath1Click(Sender: TObject);
var
  st: string;
begin
  st := EmbeddedWB1.GetHistoryPath;
  ShowMessage(st);
end;

procedure TfrmMain.GetFavoritesPath1Click(Sender: TObject);
var
  st: string;
begin
  st := EmbeddedWB1.GetHistoryPath;
  ShowMessage(st);
end;

procedure TfrmMain.ShowOrganizeFavorites1Click(Sender: TObject);
begin
  EmbeddedWB1.ShowOrganizeFavorites;
end;

procedure TfrmMain.IEAddress1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = 13) then
  begin
    UpdateControls;
    EmbeddedWB1.Go(trim(IEAddress1.Text));
  end;
end;

procedure TfrmMain.EmbeddedWB1NewWindow2(ASender: TObject;
  var ppDisp: IDispatch; var Cancel: WordBool);
begin
  AddEventLog('OnNewWindow');
  if tbBlock.Down then
    Cancel := True
  else
    EmbeddedWB1.Navigate(InitialURL);
end;

procedure TfrmMain.OpenAddressBook1Click(Sender: TObject);
begin
  EWBTools.OpenAddressBook;
end;

procedure TfrmMain.OpenMailClient1Click(Sender: TObject);
begin
  EWBTools.OpenOutlookMail;
end;

procedure TfrmMain.OpenNewsClient1Click(Sender: TObject);
begin
  EWBTools.OpenNewsClient;
end;

procedure TfrmMain.OpenCalender1Click(Sender: TObject);
begin
  EWBTools.OpenCalendar;
end;

procedure TfrmMain.OpenNetMeetingClick(Sender: TObject);
begin
  EWBTools.OpenNetMeeting;
end;

procedure TfrmMain.OpenYahooMail1Click(Sender: TObject);
begin
  EWBTools.OpenYahooMail(EmbeddedWB1);
end;

procedure TfrmMain.OpenOutlookExpress1Click(Sender: TObject);
begin
  EWBTools.OpenOutlookExpressMail;
end;

procedure TfrmMain.OpenOutlook1Click(Sender: TObject);
begin
  EWBTools.OpenOutlookMail;
end;

procedure TfrmMain.OpenGoogleMail1Click(Sender: TObject);
begin
  EWBTools.OpenGoogleMail(EmbeddedWB1);
end;

procedure TfrmMain.OpenHotmailMail1Click(Sender: TObject);
begin
  EWBTools.OpenHotmailMail(EmbeddedWB1);
end;

procedure TfrmMain.EmbeddedWB1BeforeNavigate2(ASender: TObject;
  const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData,
  Headers: OleVariant; var Cancel: WordBool);
begin
  lvEventLog.Clear;
  AddEventLog('OnBeforeNavigate2 ' + URL);
  Cancel := tbBlock.Marked;
end;

procedure TfrmMain.EmbeddedWB1NavigateComplete2(ASender: TObject;
  const pDisp: IDispatch; var URL: OleVariant);
begin
  AddEventLog('OnNavigateComplete2 ' + URL);
  IEAddress1.Text := EmbeddedWB1.LocationURL;
  UpdateSecurityZone;
  UpdateSSLStatus;
end;

procedure TfrmMain.EmbeddedWB1DocumentComplete(ASender: TObject;
  const pDisp: IDispatch; var URL: OleVariant);
begin
  AddEventLog('OnDocumentComplete: ' + URL);
end;

procedure TfrmMain.EmbeddedWB1FullScreen(ASender: TObject;
  FullScreen: WordBool);
begin
  if FullScreen then
    AddEventLog('OnFullScreen: ' + 'FullScreen');
end;

procedure TfrmMain.EmbeddedWB1Visible(ASender: TObject; Visible: WordBool);
begin
  if Visible then
    AddEventLog('OnVisible: ' + 'Visible');
end;

procedure TfrmMain.GoAboutBlank1Click(Sender: TObject);
begin
  EmbeddedWB1.GoAboutBlank;
end;

procedure TfrmMain.Godownloadafile1Click(Sender: TObject);
var
  URL: string;
begin
  UpdateControls;
  InputQuery('Download a file', 'Please Enter a valid path + file name', URL);
  EmbeddedWB1.GoDownloadFile(URL);
end;

procedure TfrmMain.GoDowloadMasked1Click(Sender: TObject);
const
  CSIDL_DESKTOPDIRECTORY = $0010;
begin
  UpdateControls;
  EmbeddedWB1.GoDownloadMaskedFile('http://www.google.com/intl/de/images/home_title.gif',
    (EmbeddedWB1.GetSpecialFolderPath(Handle, CSIDL_DESKTOPDIRECTORY)) + '\google-image.gif', True);
end;

procedure TfrmMain.checkOnlineStatus2Click(Sender: TObject);
begin
  if not EmbeddedWB1.CheckOnlineStatus then
    MessageDlg('It looks like if you are not connected to the internet!', mtWarning, mbOKCancel, 0)
  else
    MessageDlg('Connected to the internet!', mtInformation, [mbOK], 0)
end;

procedure TfrmMain.SaveThePageTofile1Click(Sender: TObject);
begin
  EmbeddedWB1.SaveToFile('Site.html');
  EmbeddedWB1.ExploreFolder(ExtractFilePath(Application.ExeName));
end;

procedure TfrmMain.SendThePageInMail1Click(Sender: TObject);
begin
  EmbeddedWB1.SendPageInMailAsAttachment(FrmMain, 'Site.html', 'Check this site', 'With regards');
end;

procedure TfrmMain.CreateNewMail1Click(Sender: TObject);
begin
  EmbeddedWB1.CreateNewMail;
end;

procedure TfrmMain.GetSpecialFolderPath1Click(Sender: TObject);
const
  CSIDL_DESKTOPDIRECTORY = $0010;
 {MoreUses: (shlobj.pas)}
 {CSIDL_DESKTOP = $0000;      CSIDL_DRIVES   = $0011;
  CSIDL_PROGRAMS = $0002;     CSIDL_NETWORK  = $0012;
  CSIDL_CONTROLS = $0003;     CSIDL_NETHOOD  = $0013;
  CSIDL_PRINTERS = $0004;     CSIDL_FONTS    = $0014;
  CSIDL_PERSONAL = $0005;     CSIDL_TEMPLATES = $0015;
  CSIDL_FAVORITES = $0006;    CSIDL_TEMPLATES = $0015;
  CSIDL_STARTUP = $0007;      CSIDL_COMMON_STARTMENU = $0016;
  CSIDL_RECENT = $0008;       CSIDL_COMMON_PROGRAMS  = $0017;
  CSIDL_SENDTO = $0009;       CSIDL_COMMON_STARTUP   = $0018;
  CSIDL_BITBUCKET = $000a;    CSIDL_COMMON_DESKTOPDIRECTORY  = $0019;
  CSIDL_STARTMENU = $000b;    CSIDL_APPDATA                  = $001A;
  CSIDL_DESKTOPDIRECTORY = $0010;    CSIDL_PRINTHOOD         = $001B;}
begin
  ShowMessage('Your Desktop Path is: ' + EmbeddedWB1.GetSpecialFolderPath(Handle, CSIDL_DESKTOPDIRECTORY));
end;

procedure TfrmMain.CreateAShortCutOnYourDeskTop1Click(Sender: TObject);
begin
  EmbeddedWB1.CreateDesktopShortcut;
end;

procedure TfrmMain.ExportFavorites1Click(Sender: TObject);
begin
  ExportFavorite1.ExportFavorites;
  if ExportFavorite1.SuccessFlag then
    EmbeddedWB1.Navigate(ExportFavorite1.NavigatePath);
end;

procedure TfrmMain.ImportFavorites1Click(Sender: TObject);
begin
  ImportFavorite1.ImportFavorites;
  UpdateView();
  if ImportFavorite1.SuccessFlag then
    EmbeddedWB1.ShowOrganizeFavorites;
end;

procedure TfrmMain.AddToFavorites1Click(Sender: TObject);
begin
  EmbeddedWB1.AddToFavorites(EmbeddedWB1.LocationURL, EmbeddedWB1.LocationName);
  UpdateView();
end;

procedure TfrmMain.FavoritesMenu1AddFavorites(const EmbeddedWB: TEmbeddedWB;
  Title, URL: WideString; Success: Integer);
begin
  ShowMessage(EmbeddedWB.GetNamePath + '' + Title + '' + URL);
end;

procedure TfrmMain.FavoritesTree1Expanded(Sender: TObject; Node: TTreeNode);
begin
  if (pos('Links', Node.Text) > 0) or (pos('Imported', Node.Text) > 0)
    then
    with node do
    begin
      HasChildren := True;
      if not Expanded then
        ImageIndex := 6
      else
        ImageIndex := 7;
    end
end;

procedure TfrmMain.GoWithQueryDetails1Click(Sender: TObject);
var
  Url, Query: string;
begin
  InputQuery('Browser Demo', 'Please enter the url', Url);
  InputQuery('Browser Demo', 'Please enter the Query', Query);
  EmbeddedWB1.GoWithQueryDetails(Url, Query);
end;

procedure TfrmMain.LoadFromStringsClick(Sender: TObject);
begin
  UpdateEditor;
  RichEditWB1.Lines.Add('<p><font size="7">This HTML is a test of: Browser - Load From Strings.  </font></p>');
  pctrlWB.Pages[1].Caption := 'Browser - Load From Strings';
  EmbeddedWB1.LoadFromStrings(RichEditWB1.Lines);
  RichEditWB1.DoHighlightHtml;
end;

procedure TfrmMain.SavePageToStrings1Click(Sender: TObject);
begin
  UpdateEditor;
  pctrlWB.Pages[1].Caption := 'Browser - Save To Strings';
  EmbeddedWB1.SaveToStrings(RichEditWB1.Lines);
end;

procedure TfrmMain.SavePageToStreamClick(Sender: TObject);
var
  Stm: TMemoryStream;
begin
  Stm := TMemoryStream.Create;
  try
    UpdateEditor;
    pctrlWB.Pages[1].Caption := 'Browser - Save To Stream';
    EmbeddedWB1.SaveToStream(Stm);
    Stm.Position := 0;
    RichEditWB1.Lines.LoadFromStream(Stm);
    RichEditWB1.DoHighlightHTML;
  finally
    Stm.Free;
  end;
end;

procedure TfrmMain.LoadFromStreamClick(Sender: TObject);
var
  Stm: TMemoryStream;
begin
  Stm := TMemoryStream.Create;
  try
    UpdateEditor;
    RichEditWB1.Lines.Add('<p><font size="7">This HTML is a test of: Browser - Load From Stream.  </font></p>');
    RichEditWB1.DoHighlightHTML;
    pctrlWB.Pages[1].Caption := 'Browser - Load From Stream';
    RichEditWB1.Lines.SaveToStream(Stm);
    Stm.Position := 0;
    EmbeddedWB1.LoadFromStream(Stm);
  finally
    Stm.Free;
  end;
end;

procedure TfrmMain.ImportExportWizard1Click(Sender: TObject);
begin
  EmbeddedWB1.ShowImportExportFavoritesAndCookies;
end;

procedure TfrmMain.ShowInternetExplorerVersion1Click(Sender: TObject);
begin
  EmbeddedWB1.ShowIEVersionInfo;
end;

procedure TfrmMain.GetHostAndIP1Click(Sender: TObject);
var
  HostN, HostIP, Err: string;
begin
  if EmbeddedWB1.GetIPAndHostName(HostN, HostIP, Err) then
    MessageDlg('PC Host Name: ' + HostN + #10 + #13 + 'Your Internal IP: ' + HostIP, mtInformation, [mbOk], 0)
  else
    MessageDlg(Err, mtError, [mbOk], 0);
end;

procedure TfrmMain.GetDefaultWebBrowserFromResistry1Click(Sender: TObject);
begin
  MessageDlg('Current Default Web Browser is: ' + EmbeddedWB1.GetDefaultBrowserFromRegistry, mtInformation, [mbOk], 0)
end;

procedure TfrmMain.ClearCache1Click(Sender: TObject);
begin
  EmbeddedWB1.ClearCache;
end;

procedure TfrmMain.DeleteHistory1Click(Sender: TObject);
begin
  EmbeddedWB1.ClearHistory;
end;

procedure TfrmMain.ClearAddressBarTypedURLs1Click(Sender: TObject);
begin
  EmbeddedWB1.ClearTypedURLs;
end;

procedure TfrmMain.PageSourceHtmlAsStrings1Click(Sender: TObject);
begin
  UpdateEditor;
  EmbeddedWB1.ViewPageSourceHtmlToStrings(RichEditWB1.Lines);
  if RichEditWB1.HighlightHTML then
    RichEditWB1.DoHighlightHTML;
end;

procedure TfrmMain.PageSourceTextAsStrings1Click(Sender: TObject);
begin
  UpdateEditor;
  EmbeddedWB1.ViewPageSourceTextToStrings(RichEditWB1.Lines);
end;

procedure TfrmMain.ViewPageLinksAsAList1Click(Sender: TObject);
begin
  UpdateEditor;
  EmbeddedWB1.ViewPageLinksToStrings(RichEditWB1.Lines);
end;

procedure TfrmMain.PageCtrlChange(Sender: TObject);
begin
  UpdateView();
end;

procedure TfrmMain.SearchImMsn1Click(Sender: TObject);
begin
  EmbeddedWB1.GoSearchInMSN('EmbeddedWB');
end;

procedure TfrmMain.SearchInGoogle1Click(Sender: TObject);
begin
  EmbeddedWB1.GoSearchInGoogle('EmbeddedWB');
end;

procedure TfrmMain.SearchInYahoo1Click(Sender: TObject);
begin
  EmbeddedWB1.GoSearchInYahoo('EmbeddedWB');
end;

procedure TfrmMain.FillFormWithPersonalDetails1Click(Sender: TObject);
begin
  EmbeddedWB1.Navigate('http://edit.yahoo.com/config/eval_register?.done=http://groups.yahoo.com%2fgroup%2fdelphi-webbrowser%2f&.src=ygrp&.intl=us');
  EmbeddedWB1.Wait;
  with EmbeddedWB1 do
  begin
    FillForm('firstname', 'Mozart');
    FillForm('secondname', 'Bethoven');
    FillForm('yahooid', 'lolypop');
    // ...
  end;
end;
///------------------Images-------------------------------------

procedure TfrmMain.ShowTheImageEditor1Click(Sender: TObject);
begin
  UpdateImage;
end;

procedure TfrmMain.getAScreanCapture1Click(Sender: TObject);
begin
  if EmbeddedWB1.GetBmpFromBrowser('site.bmp') then
  begin
    UpdateImage;
    ImageViewer.Picture.LoadFromFile('.\site.bmp');
    ShowMessage('The page screen capture is in your application folder.');
  end;
end;

procedure TfrmMain.GetThumbnailClick(Sender: TObject);
begin
  UpdateImage;
  EmbeddedWB1.GetThumbnail(ImageViewer);
end;

procedure TfrmMain.GetJpegPageCapture1Click(Sender: TObject);
begin
  if EmbeddedWB1.GetJpegFromBrowser('.\Site.jpg', Height, Width, Height, Width) then
  begin
    UpdateImage;
    ImageViewer.Picture.LoadFromFile('.\Site.jpg');
    ShowMessage('The page screen capture is in your application folder.')
  end;
end;

///------------------End of Images-------------------------------------

procedure TfrmMain.OpenFoldersExplore1Click(Sender: TObject);
begin
  EmbeddedWB1.ExploreFolder(ExtractFilePath(application.ExeName));
end;

procedure TfrmMain.OpenRegistryEditor1Click(Sender: TObject);
begin
  EWBTools.OpenRegistryEditor;
end;

procedure TfrmMain.PrintWithOptionsClick(Sender: TObject);
begin
  EmbeddedWB1.PrintWithOptions;
end;

procedure TfrmMain.AddToRestrictedZoneListClick(Sender: TObject);
begin
  EmbeddedWB1.AddToRestrictedSiteList(EmbeddedWB1.LocationURL);
end;

procedure TfrmMain.CheckIfInResrictedZoneList1Click(Sender: TObject);
begin
  if EmbeddedWB1.CheckIfInRestricredList(EmbeddedWB1.LocationURL, False) then
    MessageDlg('The page is in your Resricted sites list zone.', mtInformation, [mbOK], 0)
  else
    MessageDlg('The page is not in your Resricted sites list zone!', mtInformation, [mbOK], 0)
end;

procedure TfrmMain.AddToTrustedListZoneList1Click(Sender: TObject);
begin
  EmbeddedWB1.AddToTrustedSiteList(EmbeddedWB1.LocationURL);
end;

procedure TfrmMain.CheckIfInTrustedZoneList1Click(Sender: TObject);
begin
  if EmbeddedWB1.CheckIfInTrustedList(EmbeddedWB1.LocationURL, False) then
    MessageDlg('The page is in your truested sites list zone.', mtInformation, [mbOK], 0)
  else
    MessageDlg('The page is not in your truested sites list zone!', mtInformation, [mbOK], 0)
end;

procedure TfrmMain.CheckSiteSecurityLevel1Click(Sender: TObject);
var
  Name, Description: string;
begin
  if EmbeddedWB1.GetSSLStatus(Name, Description) then
    ShowMessage('Secure Type: ' + Name + #10 + #13 + 'Description: ' + Description)
  else
    ShowMessage('The site is not secured.');
end;

procedure TfrmMain.CheckSiteSecurityZone1Click(Sender: TObject);
var
  Name, Description: string;
  Icon: TIcon;
begin
  EmbeddedWB1.GetURLSecurityZone(Name, Description, Icon);
  ShowMessage('The Web Site Security Zone Is: ' + Name);
end;

procedure TfrmMain.CheckPageSecurityEncryption1Click(Sender: TObject);
begin
  ShowMessage(EncryptionSt);
end;

procedure TfrmMain.FavoritesTree1NodeAdded(Sender: TObject;
  const aNode: TTreeNode; aNodeType: TNodeType);
begin
  with aNode do
  begin
    if Text = 'Tools' then
      ImageIndex := 0
    else
      if Text = 'Organize favorites' then
        ImageIndex := 1
      else
        if Text = 'Add To favorites' then
          ImageIndex := 2
        else
          if Text = 'Import favorites' then
            ImageIndex := 3
          else
            if Text = 'Export favorites' then
              ImageIndex := 4
            else
              if Text = 'Favorites' then
                ImageIndex := 5
              else
                if (pos('Links', Text) > 0) or (pos('Imported', Text) > 0)
                  then
                begin
                  HasChildren := True;
                  ImageIndex := 6
                end
                else
                  ImageIndex := 8
  end;
end;

procedure TfrmMain.EmbeddedWB1ScriptError(Sender: TObject; ErrorLine,
  ErrorCharacter, ErrorCode, ErrorMessage, ErrorUrl: string;
  var ScriptErrorAction: TScriptErrorAction);
begin
  AddEventLog('OnNavigateError ' + ErrorMessage + '  ' + ErrorCode);
end;

procedure TfrmMain.EmbeddedWB1SetSecureLockIcon(ASender: TObject;
  SecureLockIcon: Integer);
begin
  case SecureLockIcon of
    0: EncryptionSt := 'UnSecure';
    1: EncryptionSt := 'Mixed';
    2: EncryptionSt := 'Unknown Bits';
    3: EncryptionSt := '40 Bit';
    4: EncryptionSt := '56 Bit';
    5: EncryptionSt := 'Fortezza';
    6: EncryptionSt := '128 Bit';
  end;
  EncryptionSt := 'Secure Level: ' + EncryptionSt;
  lblLevel.Caption := EncryptionSt;
end;

procedure TfrmMain.EmbeddedWB1ShowDialog(Sender: TObject; h: Cardinal;
  StyleEx: Integer; OldCaption: string; var NewCaption: WideString;
  var Cancel: Boolean);
begin
  AddEventLog('OnShowDialog');
end;

procedure TfrmMain.SetDesignModeClick(Sender: TObject);
begin
  EmbeddedWB1.DesignMode := not EmbeddedWB1.DesignMode;
  if EmbeddedWB1.DesignMode then
    Stbar.Panels[4].Text := 'Design Mode '
  else
    Stbar.Panels[4].Text := ''
end;

//********* LinksBar & IEDownload*********************************************

procedure TfrmMain.AddTheSiteToTheList1Click(Sender: TObject);
begin
  LinksBar1.AddToLinksList(EmbeddedWB1.LocationName, EmbeddedWB1.LocationURL);
end;

procedure TfrmMain.RemoveTheSiteFromTheList1Click(Sender: TObject);
begin //remmmber to add links to the list before using this feature
  LinksBar1.RemoveFromLinksList(EmbeddedWB1.LocationName);
end;

procedure TfrmMain.ShowTheList1Click(Sender: TObject);
var
  lst: TstringList;
  Count: integer;
begin
  Lst := TStringList.Create;
  try
    Count := 0;
    UpdateEditor;
    LinksBar1.GetTheLinksList(lst);
    RichEditWB1.Lines.AddStrings(lst);
    RichEditWB1.Lines.Add('**** You have a total of ' + IntToStr(Count) + ' links in your list****');
  finally
    Lst.Free;
  end;
end;

procedure TfrmMain.NavigateToLinkItem1Click(Sender: TObject);
var
  st: string;
begin //remmmber to add links to the list before using this feature
  st := ('delphi-webbrowser : Delphi: Using IE''s WebBrowser');
  LinksBar1.NavigateToItem(st);
end;

procedure TfrmMain.ViewHidethelinksbar1Click(Sender: TObject);
begin
  Linksbar1.Shown := not ViewHidethelinksbar1.Checked;
end;

procedure TfrmMain.ViewHideTheLinksToolbar1Click(Sender: TObject);
begin
  ViewHidethelinksbar1.Click;
end;

procedure TfrmMain.ClearTheLinkList1Click(Sender: TObject);
begin
  LinksBar1.ClearTheLinksList;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
var
  i: Integer;
begin
  ClearResultsGrid;
  for i := 0 to links.count - 1 do
    IEDownload1.Go(Links[i]);
end;

function TfrmMain.IEDownload1Response(Sender: TBSCB; dwResponseCode: Cardinal;
  szResponseHeaders, szRequestHeaders: PWideChar;
  out szAdditionalRequestHeaders: PWideChar): HRESULT;
begin
  Result := S_OK;
  if dwResponseCode = 200 then
    StringGrid1.Cells[1, Links.IndexOf(IEDownload1.Url) + 1] := 'Finished...'
  else
    if dwResponseCode > 400 then
      StringGrid1.Cells[1, Links.IndexOf(IEDownload1.Url) + 1] := ResponseCodeToStr(dwResponseCode)
    else
      StringGrid1.Cells[1, Links.IndexOf(IEDownload1.Url) + 1] := ResponseCodeToStr(Result);
  StringGrid1.Cells[2, Links.IndexOf(IEDownload1.Url) + 1] := ResponseCodeToStr(dwResponseCode);
  StringGrid1.Repaint; // For Synchronous download only
end;

procedure TfrmMain.IEDownload1Progress(Sender: TBSCB; ulProgress, ulProgressMax,
  ulStatusCode: Cardinal; szStatusText: PWideChar; Downloaded, ElapsedTime,
  Speed, RemainingTime, Status: string);
begin

  StringGrid1.Cells[1, Links.IndexOf(IEDownload1.Url) + 1] := ResponseCodeToStr(ulStatusCode);
  StringGrid1.Repaint; // For Synchronous download only
end;

procedure TfrmMain.RadioButton1Click(Sender: TObject);
begin
  IEDownload1.BindF := [GetNewestVersion, NoWriteCache, PullData];
end;

procedure TfrmMain.RadioButton2Click(Sender: TObject);
begin
  IEDownload1.BindF := [Asynchronous, AsyncStorage, GetNewestVersion, NoWriteCache, PullData];
end;

procedure TfrmMain.CheckTheLinksClick(Sender: TObject);
var
  i: Integer;
  UrlList: TStringList;
begin
  UpdateLinksChecker;
  UrlList := TStringList.Create;
  try
    ClearAllGrid;
    LinksBar1.GetTheLinksURLs(UrlList);
    Links.Clear;
    Links.AddStrings(UrlList);
    for i := 0 to links.count - 1 do
    begin
      Stringgrid1.RowCount := i + 2;
      Stringgrid1.Cells[0, i + 1] := links[i];
    end;
    for i := 0 to links.count - 1 do
      IEDownload1.Go(Links[i]);
  finally
    UrlList.Free;
  end;
end;

procedure TfrmMain.CheckURLsFromALinkList1Click(Sender: TObject);
begin
  UpdateLinksChecker;
end;

//********* local embeddedWB*********************************************

procedure TfrmMain.sbRefreshClick(Sender: TObject);
begin
  EmbeddedWB2.Refresh;
end;

procedure TfrmMain.sbUpClick(Sender: TObject);
begin
  EmbeddedWB2.GoBack;
end;

procedure TfrmMain.sbConnectClick(Sender: TObject);
begin
  if (AnsiPos('ftp', IEAddress1.Text) > 0) then
  begin
    EmbeddedWB1.Navigate('ftp://' + edtUser.text + ':' + edtPassword.text + '@' + IEAddress1.text);
    repeat
      Application.ProcessMessages;
    until not EmbeddedWB1.Busy;
  end
  else
  begin
    ShowMessage('Check your address, Its not an ftp site.' + #10 + #13
      + 'You are being redirected to Microsoft FTP just for the test.');
    EmbeddedWB1.Navigate('ftp://ftp.microsoft.com/');
  end;
end;

procedure TfrmMain.EmbeddedWB2CommandStateChange(ASender: TObject;
  Command: Integer; Enable: WordBool);
begin
  case Command of
    CSC_NAVIGATEBACK: sbUp.Enabled := Enable;
  end;
end;
//************ Editor *******************************************

procedure TfrmMain.HighlighHTML1Click(Sender: TObject);
begin
  UpdateEditor;
  RichEditWB1.LoadHTMLFromBrowser;
  if RichEditWB1.HighlightHTML then
    RichEditWB1.DoHighlightHTML;
  RichEditWB1.ScrollToTop;
end;

procedure TfrmMain.HighLightURL1Click(Sender: TObject);
begin
  RichEditWB1.HighLightURL := True;
  UpdateEditor;
  EmbeddedWB1.ViewPageLinksToStrings(RichEditWB1.Lines);
  RichEditWB1.AddLineNumbering;
end;

procedure TfrmMain.SetSelectionAsAHyperLink1Click(Sender: TObject);
begin
  RichEditWB1.SetSelectionHyperlink(True);
end;

procedure TfrmMain.SetWordAsAHyperLink1Click(Sender: TObject);
begin
  RichEditWB1.SetWordHyperlink(True);
end;

procedure TfrmMain.AddBullets1Click(Sender: TObject);
begin
  RichEditWB1.AddBullets;
end;

procedure TfrmMain.AddLineNumbers1Click(Sender: TObject);
begin
  RichEditWB1.AddLineNumbering;
end;

procedure TfrmMain.AddRomanNumbers1Click(Sender: TObject);
begin
  RichEditWB1.AddRomanNumbering;
end;

procedure TfrmMain.New1Click(Sender: TObject);
begin
  UpdateEditor;
  RichEditWB1.New;
end;

procedure TfrmMain.Open2Click(Sender: TObject);
begin
  UpdateEditor;
  RichEditWB1.Open;
end;

procedure TfrmMain.Save1Click(Sender: TObject);
begin
  UpdateEditor;
  RichEditWB1.Save;
end;

procedure TfrmMain.SaveAs2Click(Sender: TObject);
begin
  UpdateEditor;
  RichEditWB1.SaveAs;
end;

procedure TfrmMain.SetFontColorClick(Sender: TObject);
begin
  UpdateEditor;
  RichEditWB1.SetFontColor;
end;

procedure TfrmMain.AddBackroundColor1Click(Sender: TObject);
begin
  UpdateEditor;
  RichEditWB1.SetSelectedBgColor;
end;

procedure TfrmMain.SelectFonts1Click(Sender: TObject);
begin
  UpdateEditor;
  RichEditWB1.SelectFont;
end;

procedure TfrmMain.SetSize1Click(Sender: TObject);
begin
  UpdateEditor;
  RichEditWB1.SelectFont;
end;

procedure TfrmMain.SetBold1Click(Sender: TObject);
begin
  UpdateEditor;
  RichEditWB1.SetFontBold;
end;

procedure TfrmMain.SetSize2Click(Sender: TObject);
begin
  UpdateEditor;
  RichEditWB1.SetFontSize;
end;

procedure TfrmMain.SetItalic1Click(Sender: TObject);
begin
  UpdateEditor;
  RichEditWB1.SetFontItalic;
end;

procedure TfrmMain.SetUnderLine1Click(Sender: TObject);
begin
  UpdateEditor;
  RichEditWB1.SetFontUnderLine;
end;

procedure TfrmMain.AddDateAndTime1Click(Sender: TObject);
begin
  UpdateEditor;
  RichEditWB1.AddDateAndTime;
end;

procedure TfrmMain.PreviewRichEditLinesInTheBrowser1Click(Sender: TObject);
begin
  RichEditWB1.PreviewInBrowser;
end;

procedure TfrmMain.LoadCodeFromBrowserStream1Click(Sender: TObject);
begin
  UpdateEditor;
  RichEditWB1.LoadStreamFromBrowser;
end;

procedure TfrmMain.SetColor1Click(Sender: TObject);
begin
  UpdateEditor;
  RichEditWB1.SetColor;
end;

procedure TfrmMain.Print1Click(Sender: TObject);
begin
  UpdateEditor;
  RichEditWB1.PrintAll;
end;

procedure TfrmMain.Find1Click(Sender: TObject);
begin
  UpdateEditor;
  RichEditWB1.Find;
end;

procedure TfrmMain.Replace1Click(Sender: TObject);
begin
  UpdateEditor;
  RichEditWB1.Replace;
end;

procedure TfrmMain.CreateASnapshot1Click(Sender: TObject);
begin
  UpdateImage;
  RichEditWB1.CreateSnapShot(ImageViewer.Picture.Bitmap);
end;

procedure TfrmMain.AddACheckBox1Click(Sender: TObject);
begin
  UpdateEditor;
  RichEditWB1.AddCheckBox(' bsalsa', 'cb', 20, 30, 170, True);
end;

procedure TfrmMain.AddTEditBox1Click(Sender: TObject);
begin
  UpdateEditor;
  RichEditWB1.AddEditBox('bsalsa', 'edt', 20, 30, 80);
end;

procedure TfrmMain.AddARadioButton1Click(Sender: TObject);
begin
  UpdateEditor;
  RichEditWB1.AddRadioButton('bsalsa', 'rb', 20, 30, 40, True);
end;

procedure TfrmMain.AddAButton1Click(Sender: TObject);
begin
  UpdateEditor;
  RichEditWB1.AddButton('bsalsa', 'btn', 20, 30, 130);
end;

procedure TfrmMain.GoToLineNumber1Click(Sender: TObject);
var
  Value: string;
begin
  UpdateEditor;
  InputQuery('Select A Line', 'Please Enter a line number to select..', Value);
  if Value <> '' then
  try
    RichEditWB1.SelectLine(StrToInt(Trim(Value)));
  except
  end;
end;

procedure TfrmMain.WrapLongLines1Click(Sender: TObject);
begin
  UpdateEditor;
  RichEditWB1.WordWrap := WrapLongLines1.Checked;
end;

procedure TfrmMain.MailSelectedText1Click(Sender: TObject);
begin
  UpdateEditor;
  RichEditWB1.MailSelected;
end;

procedure TfrmMain.miSaveClick(Sender: TObject);
begin
  EmbeddedWB1.SaveDialogEx;
end;

procedure TfrmMain.Mail1Click(Sender: TObject);
begin
  UpdateEditor;
  RichEditWB1.MailContext;
end;

procedure TfrmMain.RichEditWB1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  UpdateProgressBars;
end;

procedure TfrmMain.RichEditWB1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  UpdateProgressBars;
end;

procedure TfrmMain.hemes1Click(Sender: TObject);
begin
  UpdateEditor;
  RichEditWB1.SetThemes(tBlack);
end;

procedure TfrmMain.ShowTheEditor2Click(Sender: TObject);
begin
  UpdateEditor;
end;

procedure TfrmMain.ResetFontsFormat1Click(Sender: TObject);
begin
  RichEditWB1.RemoveTextFormats;
end;

procedure TfrmMain.HighLightXML1Click(Sender: TObject);
begin
  UpdateEditor;
  with RichEditWB1 do
  begin
    HighLightURL := False;
    EmbeddedWB1.Navigate('http://rss.groups.yahoo.com/group/delphi-webbrowser/rss');
    LoadAsCopyFromBrowser;
    if HighlightXML then
      DoHighlightXML;
  end;
end;

procedure TfrmMain.InsertFromImageListClick(Sender: TObject);
begin
  UpdateEditor;
  with RichEditWB1 do
  begin
    AddBitmapFromImagelist(ilsSmilies, 0);
    AddEmptyLine;
    SelAttributes.Color := clYellow;
    AddEmptyLine;
    SelText := ' Beatles ';
    AddEmptyLine;
    AddBitmapFromImagelist(ilsSmilies, 15);
    AddEmptyLine;
    SelAttributes.Color := clPurple;
    AddEmptyLine;
    SelText := ' Deep Purple ';
    AddEmptyLine;
    AddBitmapFromImagelist(ilsSmilies, 5);
    AddEmptyLine;
    AddFormatedText('Led Zepelin', True, True, True, True, clred);
    AddEmptyLine;
    AddEmptyLine;
    AddFormatedText('C U L8r', True, False, False, False, clGreen);
    AddEmptyLine;
    RemoveTextFormats;
  end;
end;

procedure TfrmMain.InsertFileClick(Sender: TObject);
begin
  UpdateEditor;
  if OpenDialog1.Execute then
    RichEditWB1.AddFile(OpenDialog1.FileName, True, True);
end;

procedure TfrmMain.InsertBitmapClick(Sender: TObject);
begin
  UpdateImage;
  UpdateEditor;
  if OpenDialog1.Execute then
    ImageViewer.Picture.LoadFromFile(OpenDialog1.FileName);
  RichEditWB1.AddFile(OpenDialog1.FileName, False, False);
end;


end. //************ E O F *******************************************

