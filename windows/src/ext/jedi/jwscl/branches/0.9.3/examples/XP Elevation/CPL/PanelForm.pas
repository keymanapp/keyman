unit PanelForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ImgList, ExtCtrls, ComCtrls;

type
  TPanel = class(TForm)
    ImageList1: TImageList;
    AddBitBtn: TBitBtn;
    RemoveBitBtn: TBitBtn;
    CheckBox1: TCheckBox;
    ListViewUsers: TListView;
    BitBtn3: TBitBtn;
    Timer: TTimer;
    NoUsersStaticText: TStaticText;
    procedure TimerTimer(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure AddBitBtnClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FirstActivation : Boolean;
    procedure UpdateList;
    function CheckRegistryKeys : Boolean;
  public
    { Public-Deklarationen }
  end;

var
  Panel: TPanel = nil;

implementation
uses Registry, JwaWindows, JwsclSID, JwsclSecureObjects;

{$R *.dfm}

{ TPanel }

procedure TPanel.UpdateList;
var Reg: TRegistry;
    SID: TJwSecurityId;
    SidStrings: TStringlist;
    i : integer;
    Item : TListItem;

    B1, B2 : Boolean;
begin
  try


  if not Assigned(AddBitBtn) or
     not Assigned(RemoveBitBtn) or
     not Assigned(NoUsersStaticText)
     then     exit;
  B1 := AddBitBtn.Enabled;
  B2 := RemoveBitBtn.Enabled;

  AddBitBtn.Enabled := false;
  RemoveBitBtn.Enabled := false;
  Reg := TRegistry.Create(KEY_QUERY_VALUE or KEY_WOW64_64KEY);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('Software\XPElevation\AllowedUsers\', false) then
    try
      SidStrings:=TStringlist.Create;
      try
        Reg.GetValueNames(SidStrings);

        if SidStrings.Count <> ListViewUsers.Items.Count then
        begin
          ListViewUsers.Items.Clear;
          for i:=0 to SidStrings.Count-1 do
          begin
            Item := ListViewUsers.Items.Add;
            try
              SID:=TJwSecurityId.Create(SidStrings[i]);
              try
                try
                  Item.Caption := SID.AccountName[''];
                except
                end;

                try
                  Item.SubItems.Add(SID.GetAccountDomainName(''));
                except
                end;

                try
                  Item.SubItems.Add(SID.StringSID);
                except
                end;

              finally
                SID.Free;
              end;
            except
              ListViewUsers.Items.Delete(Item.Index);
            end;
          end;
        end;
      finally
        SidStrings.Free;
      end;
    finally
     Reg.CloseKey;
    end;
  finally
    Reg.Free;

    AddBitBtn.Enabled := B1;
    RemoveBitBtn.Enabled := B2;
    NoUsersStaticText.Visible := ListViewUsers.Items.Count = 0;
  end;

  except
    on e : Exception do
    begin
      Timer.Enabled := false;
      MessageDlg(E.Message,mtError,[mbok],0);
      Close;
    end;
  end;
end;



procedure TPanel.TimerTimer(Sender: TObject);
begin
  UpdateList;
end;

procedure TPanel.FormActivate(Sender: TObject);
begin
  if not CheckRegistryKeys then
    exit;
    
  UpdateList;

  if not FirstActivation then
    Timer.Enabled := true;
  FirstActivation := true;
end;

procedure TPanel.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Timer.Enabled := false;
end;

procedure TPanel.FormCreate(Sender: TObject);
begin
  FirstActivation := false;
end;

procedure TPanel.BitBtn3Click(Sender: TObject);
begin
  Close;
end;

procedure TPanel.AddBitBtnClick(Sender: TObject);
var secKey : TJwSecureRegistryKey;
begin
  secKey := TJwSecureRegistryKey.Create('HKEY_LOCAL_MACHINE\Software\XPElevation\AllowedUsers');
  try
    if not secKey.AccessCheck(GENERIC_WRITE) then
    begin
      MessageDlg('You do not have the rights to add users.',mtError,[mbok],0);
      exit;
    end;
  finally
    secKey.Free; 
  end;
end;

function TPanel.CheckRegistryKeys : Boolean;
var Reg : TRegistry;
begin
  MessageDlg('1',mtError,[mbok],0);
  result := true;
  Reg := TRegistry.Create(KEY_READ or KEY_ENUMERATE_SUB_KEYS or  KEY_CREATE_SUB_KEY);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if not Reg.KeyExists('Software\XPElevation\AllowedUsers') then
    begin
      MessageDlg('2',mtError,[mbok],0);
      try
        Reg.CreateKey('Software\XPElevation');
        Reg.CreateKey('Software\XPElevation\AllowedUsers');
      except
        result := false;
        MessageDlg('Installation of registry keys failed.',mtError,[mbok],0);
        Close;
      end;
    end;
  finally
    Reg.Free;
  end;       
end;

end.
