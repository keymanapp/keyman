unit uMain;

interface

uses
  Windows, Forms, Messages, StdCtrls, Buttons,
  ExtCtrls, Classes, SysUtils, OleCtrls, SHDocVw_EWB, EwbCore, EmbeddedWB, Controls,
  ComCtrls, ImgList, Graphics;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    ImageList1: TImageList;
    EmbeddedWB1: TEmbeddedWB;
    ListView1: TListView;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EmbeddedWB1PreRefresh(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  ShellApi;

procedure LV_InsertFiles(strPath: string; ListView: TListView; ImageList: TImageList);
var
  i: Integer;
  Icon: TIcon;
  SearchRec: TSearchRec;
  ListItem: TListItem;
  FileInfo: SHFILEINFO;
begin
  // Create a temporary TIcon
  Icon := TIcon.Create;
  ListView.Items.BeginUpdate;
  try
    // search for the first file
    i := FindFirst(strPath + '*.*', faAnyFile, SearchRec);
    while i = 0 do
    begin
      with ListView do
      begin
        // On directories and volumes
        if ((SearchRec.Attr and FaDirectory <> FaDirectory) and
          (SearchRec.Attr and FaVolumeId <> FaVolumeID)) then
        begin
          ListItem := ListView.Items.Add;
          //Get The DisplayName
          SHGetFileInfo(PChar(strPath + SearchRec.Name), 0, FileInfo,
            SizeOf(FileInfo), SHGFI_DISPLAYNAME);
          Listitem.Caption := FileInfo.szDisplayName;
          // Get The TypeName
          SHGetFileInfo(PChar(strPath + SearchRec.Name), 0, FileInfo,
            SizeOf(FileInfo), SHGFI_TYPENAME);
          ListItem.SubItems.Add(FileInfo.szTypeName);
          //Get The Icon That Represents The File
          SHGetFileInfo(PChar(strPath + SearchRec.Name), 0, FileInfo,
            SizeOf(FileInfo), SHGFI_ICON or SHGFI_SMALLICON);
          icon.Handle := FileInfo.hIcon;
          ListItem.ImageIndex := ImageList.AddIcon(Icon);
          // Destroy the Icon
          DestroyIcon(FileInfo.hIcon);
        end;
      end;
      i := FindNext(SearchRec);
    end;
  finally
    Icon.Free;
    ListView.Items.EndUpdate;
  end;
end;

procedure TForm1.EmbeddedWB1PreRefresh(Sender: TObject);
var
  Path: string;
begin
  ListView1.Visible := EmbeddedWB1.GetIEWin('SysListView32') <> 0;
  if ListView1.Visible then
  begin
    Path := StringReplace(EmbeddedWB1.LocationURL,'file:///','',[rfIgnoreCase]);
    Path := StringReplace(Path,'/','\',[rfIgnoreCase]);
    ImageList1.Clear;
    LV_InsertFiles(Path, ListView1, ImageList1);
  end;
  EmbeddedWB1.Enabled := not ListView1.Visible;
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  EmbeddedWB1.Go('c:\');
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
begin
  EmbeddedWB1.Go('www.google.com');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ListView1.Align := alClient;
end;

end.

 